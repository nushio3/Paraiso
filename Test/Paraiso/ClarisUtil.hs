{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.ClarisUtil (
  evaluate
  ) where

import           Control.Concurrent         (forkIO)
import           Control.Monad
import           Data.List (isSuffixOf)
import           Data.Tensor.TypeLevel
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Claris      as C
import qualified Language.Paraiso.Generator.ClarisTrans as C
import qualified Language.Paraiso.Generator.Native      as Native
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude
import           Prelude hiding ((++))
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))
import           System.IO                  (hGetLine, hIsEOF, Handle)
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Process             (createProcess, CreateProcess(..),
                                             system, shell, StdStream(..), waitForProcess)
import           System.Random              (randomIO)
import qualified Test.Paraiso.Option                    as Option


evaluate :: C.Program -> Int
evaluate prog = unsafePerformIO $ do
  key <- randomIO
  let path :: FilePath
      path = "/tmp/" ++ (show :: Int -> String) key
      exeFn = path </> "dragon.out" -- roar!
      filetasks = [ (headerFn, C.translate C.headerFile prog),
                    (cppFn   , C.translate C.sourceFile prog)
                  ]
      headerFn  = nameStr prog ++ ".hpp"
      cppFn     = nameStr prog ++ ".cpp"

  createDirectoryIfMissing True path
  files <- forM filetasks $ \ (fn, con) -> do
    let absfn = path </> fn
    T.writeFile absfn con
    return (absfn, con)


  let cppFn :: FilePath
      cppFn = head $ filter (isSuffixOf ".cpp") $ map fst $ files
  _ <- system $ unwords [Option.cppc, "-O3",  cppFn, "-I", path,  "-o",  exeFn]
  (_, Just hout, _, handle) <- createProcess (shell exeFn) {std_out = CreatePipe}
  ret <- fmap (read :: String -> Int) $ hGetLine hout
  _ <- forkIO $ suckAll hout
  _ <- waitForProcess handle
  _ <- system $ "rm -fr " ++ path
  return ret


suckAll :: Handle -> IO ()
suckAll hdl = do
  eof <- hIsEOF hdl
  if eof
    then return ()
    else hGetLine hdl >> suckAll hdl
