{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..)
  ) where

import qualified Algebra.Ring as Ring
import Language.Paraiso.OM.Graph
import Language.Paraiso.Tensor
import NumericPrelude

-- | POM is Primordial Orthotope Machine.
data (Vector vector, Ring.C gauge) => POM vector gauge a = 
  POM {
    setup :: Setup,
    kernels :: [Kernel vector gauge a]
  } 
    deriving (Show)





