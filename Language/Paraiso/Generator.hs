{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | a general code generator definition.
module Language.Paraiso.Generator
    (
     Generator(..)
    ) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Language.Paraiso.Annotation (Annotation)
import Language.Paraiso.OM
import Language.Paraiso.Tensor (Vector)

-- | The definition for library generator.
class Generator src where
  -- | Generate the library under a certain directory.
  generate 
    :: src           -- ^The object from which the library shall be generated.
    -> FilePath      -- ^The directory name under which the files are to be generated.
    -> IO [FilePath] -- ^The act of generation. Returns the list of generated files.

