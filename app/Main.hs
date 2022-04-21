{-# LANGUAGE OverloadedStrings #-}

module Main where

import ByteCode
import Control.Exception
import Interpreter

example :: ByteCode Value
example = do
  litI 1
  write x
  litI 2
  write y
  load x
  litI 1
  add
  load y
  multiply
  ret
  where
    x = "x"
    y = "y"

main :: IO ()
main = case runByteCode example of
  Left err -> throw err
  Right res -> putStrLn $ "Example: result = " <> show res
