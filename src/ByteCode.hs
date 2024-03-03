{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module ByteCode
  ( Var,
    Value (..),
    OpCode (..),
    Channel,
    Future (..),
    getChan,
    sendChan,
    recvChan,

    -- * DSL

    -- ** Datatype
    ByteCodeF (..),
    ByteCode,

    -- ** Building blocks
    lit,
    litI,
    litB,
    load,
    write,
    loop,
    newChan,
    send,
    recv,
    fork,
    await,
    ret,

    -- *** Helpers
    add,
    multiply,
    lessThan,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (Exception)
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)

newtype Var = Var {_unVar :: Text}
  deriving newtype (Show, Eq, Ord, IsString)

data Value
  = B Bool
  | I Integer
  deriving stock (Eq, Show, Ord)

newtype Chan a = Chan {_unChan :: MVar a}
  deriving newtype (Eq)

getChan :: IO (Chan a)
getChan = Chan <$> newEmptyMVar

sendChan :: Chan a -> a -> IO ()
sendChan (Chan m) = putMVar m

recvChan :: Chan a -> IO a
recvChan (Chan m) = readMVar m

type Channel = Chan Value

type Future :: Type -> Type
data Future a where
  Future :: (Exception e) => Async (Either e a) -> Future a

data OpCode
  = Add
  | Multiply
  | LessThan
  deriving stock (Show, Eq)

data ByteCodeF next
  = Lit Value next
  | Load Var next
  | Write Var next
  | BinaryOp OpCode next
  | Loop (ByteCode Bool) (ByteCode ()) next
  | Ret (Value -> next)
  | NewChan (Channel -> next)
  | Send Channel next
  | Recv Channel next
  | Fork (ByteCode ()) (Future () -> next)
  | Await (Future ()) next
  deriving stock (Functor)

type ByteCode = Free ByteCodeF

$(makeFree ''ByteCodeF)

{-
lit      :: MonadFree ByteCodeF m => Value -> m ()
load     :: MonadFree ByteCodeF m => Var -> m ()
write    :: MonadFree ByteCodeF m => Var -> m ()
loop     :: MonadFree ByteCodeF m => ByteCode Value -> ByteCode () -> m ()
newChan  :: MonadFree ByteCodeF m => m Channel
send     :: MonadFree ByteCodeF m => Channel -> m ()
recv     :: MonadFree ByteCodeF m => Channel -> m ()
fork     :: MonadFree ByteCodeF m => ByteCode () -> m (Async ())
await    :: MonadFree ByteCodeF m => Async () -> m ()
-}

litI :: (MonadFree ByteCodeF m) => Integer -> m ()
litI = lit . I

litB :: (MonadFree ByteCodeF m) => Bool -> m ()
litB = lit . B

add :: (MonadFree ByteCodeF m) => m ()
add = binaryOp Add

multiply :: (MonadFree ByteCodeF m) => m ()
multiply = binaryOp Multiply

lessThan :: (MonadFree ByteCodeF m) => m ()
lessThan = binaryOp LessThan
