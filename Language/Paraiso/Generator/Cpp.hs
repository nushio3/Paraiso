{-# LANGUAGE  OverloadedStrings, NoImplicitPrelude  #-}
{-# OPTIONS -Wall #-}
-- | a code generator definition for single-core c++ program.
module Language.Paraiso.Generator.Cpp
    (
      Cpp(..)
    ) where

import qualified Data.Text.IO as T
import           Language.Paraiso.Name (nameStr, nameText)
import           Language.Paraiso.Prelude
import           Language.Paraiso.Generator
import qualified Language.Paraiso.Generator.Claris as C
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))


data Cpp = Cpp {openMP :: Bool}

instance Generator Cpp where
  generate me om path = do
    let 
      claris = C.translate om
      headerFn = nameStr om ++ ".hpp"
      cppFn = nameStr om ++ ".cpp"
    createDirectoryIfMissing True path
    T.writeFile (path </> headerFn) $ genHeader claris
    T.writeFile (path </> cppFn) $ genCpp claris

genHeader :: C.Program v g a -> Text
genHeader _ = "// Header\n"

genCpp :: C.Program v g a -> Text
genCpp _ = "// C++ Program\n"
