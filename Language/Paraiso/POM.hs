{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..), StaticID, Kernel(..)
  ) where

import qualified Algebra.Ring as Ring
import Language.Paraiso.OM.Graph
import Language.Paraiso.Tensor
import NumericPrelude

-- | POM is Primordial Orthotope Machine.
data (Vector vector, Ring.C gauge) => POM vector gauge = 
  POM {
    staticIDs :: [StaticValue]
  } deriving (Show)

-- | A Kernel for OM.
data (Vector vector, Ring.C gauge) => Kernel vector gauge a = 
  Kernel {
    dataflow :: OMGraph vector gauge a
  }




