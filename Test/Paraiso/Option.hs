{-# OPTIONS -Wall #-}
module Test.Paraiso.Option ( 
  cpp, cuda, fail, help,
  cppc, cudac, argv, printHelp
  ) where

import Prelude hiding (fail)
import System.Environment (getArgs)
import System.IO.Unsafe   (unsafePerformIO)

argv0 :: [String]
argv0 = unsafePerformIO $ getArgs

myArgvs :: [(String, String)]
myArgvs = [
  ("--cpp", "execute tests that invokes external c++ compiler"),
  ("--cuda", "execute tests that invokes external cuda compiler"),
  ("--fail", "all tests should fail instead of succeed when this flag is on")]

argv :: [String]
argv = filter (not . (`elem` map fst myArgvs)) argv0

cpp :: Bool 
cpp = elem "--cpp" argv0

cuda :: Bool 
cuda = elem "--cuda" argv0

fail :: Bool
fail = elem "--fail" argv0

help :: Bool
help = elem "--help" argv0

printHelp :: IO () 
printHelp = do
  putStrLn $ unlines $ header:lins  
    where
    len1 = maximum $ map ((+2) . length . fst) myArgvs 
    pad str = take len1 $ str ++ repeat ' '
    paddy (opt, desc) = pad opt ++ desc
    lins = map paddy myArgvs
    header = "**** Custom test options ****"


cppc :: FilePath -- external c++ compiler to use.
cppc = "g++"

cudac :: FilePath -- external cuda compiler to use.
cudac = "nvcc"