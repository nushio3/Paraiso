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

-- | The definition for code generator.
class Generator gen where
  -- | Code generation.
  generate :: (Vector v, Ring.C g, Additive.C (v g), Ord (v g)) =>
              gen                -- ^The code generator configuration.
           -> OM v g Annotation  -- ^The 'POM' sourcecode, annotated with 'Annotation'.
           -> FilePath           -- ^The directory name under which the files are to be generated.
           -> IO ()               -- ^The act of generation.

