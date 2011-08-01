#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import qualified Claris
import qualified Cpp
import qualified Data.ByteString as BS
import           Data.ListLike.Text ()
import qualified Data.Text.IO as T

myProgram :: Claris.Program
myProgram =  Claris.Program

myConfig :: Cpp.Config
myConfig =  Cpp.Config

main :: IO ()
main = do
  putStrLn $ Claris.translate myConfig myProgram
  BS.putStrLn $ Claris.translate myConfig myProgram
  T.putStrLn $ Claris.translate myConfig myProgram


