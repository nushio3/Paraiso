{-# LANGUAGE CPP,  OverloadedStrings  #-}
{-# OPTIONS -Wall #-}
-- | a code generator definition for single-core c++ program.
module Language.Paraiso.Generator.Cpp
    (
      Cpp(..)
    ) where

import qualified Data.Text.IO as T
import           Data.ListLike.Text ()
import           Language.Paraiso.Name (mkName, nameStr)
import           Language.Paraiso.Prelude
import           Language.Paraiso.Generator
import           Language.Paraiso.Generator.Claris as C
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))


data Cpp = Cpp {openMP :: Bool}

instance Generator Cpp where
  generate me om path = do
    let 
      claris = C.Program {
        progName = mkName "hello",
        topLevel = []
        }
      headerFn = nameStr om ++ ".hpp"
      cppFn = nameStr om ++ ".cpp"
    createDirectoryIfMissing True path
    T.writeFile (path </> headerFn) $ genHeader claris
    T.writeFile (path </> cppFn) $ genCpp claris

genHeader :: C.Program -> Text
genHeader _ = "// Header\n"

genCpp :: C.Program -> Text
genCpp _ = "// C++ Program\n"
