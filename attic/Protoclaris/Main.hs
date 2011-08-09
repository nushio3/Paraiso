#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import qualified Claris
import qualified Data.Text.IO as T

import Sample (helloWorld)

main :: IO ()
main = do
  T.putStrLn $ Claris.translate helloWorld
  


