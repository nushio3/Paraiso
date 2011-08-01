{-# OPTIONS -Wall #-}

import qualified Claris
import qualified Cpp
import qualified Data.ListLike as LL
import           Data.Text.IO as T

myProgram :: Claris.Program
myProgram =  Claris.Program

myConfig :: Cpp.Config
myConfig =  Cpp.Config

main :: IO ()
main = do
  T.putStrLn $ Claris.translate myConfig myProgram


