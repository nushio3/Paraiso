{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the Orthotope Machine program to native languages
--  with capability to describe classes and containers, such as
--  C++. Claris may include


module Language.Paraiso.Generator.Claris (      
  module Language.Paraiso.Generator.ClarisDef,
  module Language.Paraiso.Generator.ClarisTrans
  ) where

import qualified Data.Text.IO as T
import           Language.Paraiso.Generator 
import           Language.Paraiso.Generator.ClarisDef 
import           Language.Paraiso.Generator.ClarisTrans 
import           Language.Paraiso.Name (nameStr)
import           Language.Paraiso.Prelude
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))

instance Generator Program where
  generate prog path = do
  let 
    pnt, headerFn, cppFn :: FilePath
    pnt      = path </> nameStr prog
    headerFn = pnt ++ ".hpp"
    cppFn    = pnt ++ ".cpp"
  createDirectoryIfMissing True path
  T.writeFile headerFn $ translate prog
  T.writeFile cppFn    $ translate prog
  return [headerFn, cppFn]
