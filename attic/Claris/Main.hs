#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import qualified Claris
import qualified Cpp
import qualified Data.Text.IO as T

myProgram :: Claris.Program
myProgram =  Claris.Program "hello" []

myConfig :: Cpp.Config
myConfig =  Cpp.Config

main :: IO ()
main = do
  T.putStrLn $ Claris.translate myConfig myProgram


