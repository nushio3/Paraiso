{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | a general code generator definition.
module Language.Paraiso.Generator
    (
     Generator(..)
    ) where

import           Language.Paraiso.Prelude
import qualified Language.Paraiso.Generator.Native as Native
import qualified Data.Text.IO as T
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))


-- | The definition for library generator.
class Generator src where
  -- | Generate the library under a certain directory.
  generate 
    :: Native.Setup       -- ^The Library Setup. 
    -> src                -- ^The object from which the library shall be generated.
    -> [(FilePath, Text)] -- ^Returns the list of relative filenames and their contents to be generated.

  generateIO
    :: Native.Setup          -- ^The Library Setup. 
    -> src                   -- ^The object from which the library shall be generated.
    -> IO [(FilePath, Text)] -- ^The act of generation. Also returns the absolute path and contents of generated files.

  generateIO setup src = do
    let dir = Native.directory setup
    createDirectoryIfMissing True dir
    forM (generate setup src) $ \ (fn, con) -> do
      let absfn = dir </> fn
      T.writeFile absfn con
      return (absfn, con)
