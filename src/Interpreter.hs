{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter
  ( runByteCode,
    Err (..),
  )
where

import ByteCode
import Control.Applicative (liftA2)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

data Err
  = VariableNotFound Var
  | StackIsEmpty
  | BinaryOpExpectedTwoOperands
  | AsyncException Text
  | WhoNeedsTypes
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

data Ctx = Ctx
  { stack :: [Value],
    variables :: Map Var Value
  }

emptyCtx :: Ctx
emptyCtx = Ctx {stack = [], variables = M.empty}

newtype Interpreter a = Interpreter {runInterpreter :: StateT Ctx (ExceptT Err IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Ctx,
      MonadError Err,
      MonadIO,
      MonadFail
    )

popI :: Interpreter Value
popI = do
  Ctx {..} <- get
  case stack of
    [] -> throwError StackIsEmpty
    (x : xs) -> put Ctx {stack = xs, ..} >> return x

pushI :: Value -> Interpreter ()
pushI i = do
  Ctx {..} <- get
  put Ctx {stack = i : stack, ..}

loadI :: Var -> Interpreter Value
loadI var = do
  Ctx {..} <- get
  case M.lookup var variables of
    Nothing -> throwError $ VariableNotFound var
    Just v -> return v

storeI :: Var -> Value -> Interpreter ()
storeI var v = do
  Ctx {..} <- get
  let variables' = M.insert var v variables
  put Ctx {variables = variables', ..}

future :: ByteCode a -> Interpreter (Future a)
future code = do
  ctx <- get
  async' <- liftIO . async . runExceptT $ evalStateT (runInterpreter (interpret code)) ctx
  return $ Future async'

applyOp :: OpCode -> Value -> Value -> Either Err Value
applyOp Add (I i1) (I i2) = Right $ I (i1 + i2)
applyOp Multiply (I i1) (I i2) = Right $ I (i1 * i2)
applyOp LessThan (I i1) (I i2) = Right $ B (i1 < i2)
applyOp _ _ _ = Left WhoNeedsTypes

interpret :: ByteCode a -> Interpreter a
interpret = iterM algebra
  where
    algebra :: ByteCodeF (Interpreter a) -> Interpreter a
    algebra = \case
      Ret f -> popI >>= f
      Lit i k -> pushI i >> k
      Load var k -> loadI var >>= pushI >> k
      Write var k -> popI >>= storeI var >> k
      BinaryOp op k ->
        do
          catchError
            (liftA2 (applyOp op) popI popI >>= either throwError pushI)
            ( \case
                StackIsEmpty -> throwError BinaryOpExpectedTwoOperands
                e -> throwError e
            )
          >> k
      Loop cond expr k ->
        fix $ \rec -> do
          b <- interpret cond
          if b
            then interpret expr >> rec
            else k
      NewChan f ->
        liftIO getChan >>= f
      Send chan k ->
        popI >>= (liftIO . sendChan chan) >> k
      Recv chan k ->
        liftIO (recvChan chan) >>= pushI >> k
      Fork branch k ->
        future branch >>= k
      Await (Future async') k -> do
        ea <- liftIO $ waitCatch async'
        case ea of
          Left (SomeException ex) -> throwError (AsyncException (T.pack $ show ex))
          Right (Left ex) -> throwError (AsyncException (T.pack $ show ex))
          Right _r -> k

runByteCode :: ByteCode a -> Either Err a
runByteCode = unsafePerformIO . runExceptT . flip evalStateT emptyCtx . runInterpreter . interpret
