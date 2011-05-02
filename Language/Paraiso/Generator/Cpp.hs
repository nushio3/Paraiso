{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..)
    ) where
import Language.Paraiso.Generator
import System.Directory
import System.FilePath

data Cpp = Cpp

instance Generator Cpp where
  generate _ _ path = do
    createDirectoryIfMissing True path
    writeFile (path </> "test.h") "tabula rasa\n"

