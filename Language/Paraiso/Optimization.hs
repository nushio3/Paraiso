{-# LANGUAGE KindSignatures, NoImplicitPrelude, RankNTypes #-}
{-# OPTIONS -Wall #-}

-- | tipycal optimization menu

module Language.Paraiso.Optimization (
  optimize,
  OptimizationLevel(..)
  ) where

import qualified Algebra.Additive            as Additive
import           Data.Typeable
import           Language.Paraiso.Annotation
import           Language.Paraiso.Annotation.Boundary
import           Language.Paraiso.Interval
import           Language.Paraiso.OM
import           Language.Paraiso.Optimization.BoundaryAnalysis
import           Language.Paraiso.Optimization.DecideAllocation
import           Language.Paraiso.Optimization.Graph
import           Language.Paraiso.Optimization.Identity
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor (Vector)

optimize :: (Vector v, 
             Additive.C g, 
             Ord g, 
             Typeable g) => 
            OptimizationLevel -> OM v g Annotation -> OM v g Annotation
optimize level = case level of
  O0 -> gmap identity . gmap boundaryAnalysis . gmap decideAllocation
  _  -> optimize O0

data OptimizationLevel 
  = O0 
  | O1
  | O2
  | O3
