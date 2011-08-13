{-# LANGUAGE KindSignatures, NoImplicitPrelude, RankNTypes #-}
{-# OPTIONS -Wall #-}

-- | tipycal optimization menu

module Language.Paraiso.Optimization (
  OptimizationLevel(..)
  ) where

import           Language.Paraiso.Optimization.Graph

data OptimizationLevel 
  = O0 
  | O1
  | O2
  | O3
