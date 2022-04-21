{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module InterpreterSpec where

import ByteCode
import Interpreter
import Test.Hspec

spec :: Spec
spec = do
  describe "runByteCode" $ do
    describe "good" $ do
      it "returns the output of good1" $ do
        runByteCode good1 `shouldBe` Right (I 4)
      it "returns the output of good2" $ do
        runByteCode good2 `shouldBe` Right (I 10)
      it "returns the output of good3" $ do
        runByteCode good3 `shouldBe` Right (I 2)

    describe "bad" $ do
      it "returns the error of bad1" $ do
        runByteCode bad1 `shouldBe` Left (VariableNotFound "x")
      it "returns the error of bad2" $ do
        runByteCode bad2 `shouldBe` Left StackIsEmpty
      it "returns the error of bad3" $ do
        runByteCode bad3 `shouldBe` Left BinaryOpExpectedTwoOperands

good1 :: ByteCode Value
good1 = do
  litI 1
  let x = "x"
  write x
  litI 2
  let y = "y"
  write y
  load x
  litI 1
  add
  load y
  multiply
  ret

loopN :: Integer -> ByteCode ()
loopN until = do
  litI until
  write n
  litI 1
  write i
  loop (i < n) (i ++)
  where
    n = "n"
    i = "i"
    (<) i n = do
      load n
      load i
      lessThan
      v <- ret
      return $ case v of
        B b -> b
        _ -> error "unexpected"
    (++) i = do
      litI 1
      load i
      add
      write i

good2 :: ByteCode Value
good2 = do
  loopN 10
  let i = "i"
  load i
  ret

good3 :: ByteCode Value
good3 = do
  chan1 <- newChan
  chan2 <- newChan
  _ <- fork $ do
    loopN 100000
    litI 1
    send chan1
  _ <- fork $ do
    loopN 100000
    litI 1
    send chan2
  loopN 10
  recv chan1
  recv chan2
  add
  ret

bad1 :: ByteCode Value
bad1 = do
  load "x"
  ret

bad2 :: ByteCode Value
bad2 = do
  write "x"
  load "x"
  ret

bad3 :: ByteCode Value
bad3 = do
  litI 1
  add
  ret
