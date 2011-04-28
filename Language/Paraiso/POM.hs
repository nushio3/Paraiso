{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..), StaticID(..), Kernel(..)
  ) where

import qualified Algebra.Ring as Ring
import qualified Data.Graph.Inductive as G
import Language.Paraiso.POM.Graph
import Language.Paraiso.Tensor
import NumericPrelude

-- | POM is Primordial Orthotope Machine.
data (Vector vector, Ring.C gauge) => POM vector gauge = 
  POM {
    staticIDs :: [StaticID]
  } deriving (Show)

-- | A Kernel for POM.
data (Vector vector, Ring.C gauge) => Kernel vector gauge = 
  Kernel {
    dataflow :: G.Gr (POMNode vector gauge) ()
  }




