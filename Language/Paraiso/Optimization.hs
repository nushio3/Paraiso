{-# LANGUAGE KindSignatures, NoImplicitPrelude, RankNTypes #-}
{-# OPTIONS -Wall #-}

-- | tipycal optimization menu

module Language.Paraiso.Optimization (
  optimize,
  OptimizationLevel(..)
  ) where

import           Language.Paraiso.Annotation
import           Language.Paraiso.OM
import           Language.Paraiso.Optimization.DecideAllocation
import           Language.Paraiso.Optimization.Graph
import           Language.Paraiso.Optimization.Identity
import           Language.Paraiso.Prelude

optimize :: OptimizationLevel -> OM v g Annotation -> OM v g Annotation
optimize level = case level of
  O0 -> gmap identity . gmap decideAllocation
  _  -> optimize O0

data OptimizationLevel 
  = O0 
  | O1
  | O2
  | O3
