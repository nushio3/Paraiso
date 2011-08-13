{-# LANGUAGE KindSignatures, NoImplicitPrelude, RankNTypes #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Optimization.Graph (
  Optimization, OptimizationEx, gmap
  ) where

import qualified Data.Vector as V
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM
import           Language.Paraiso.Prelude


-- | the most frequent type of optimization is which 
type Optimization   = forall (v :: * -> *) (g :: *). Graph v g Annotation -> Graph v g Annotation
type OptimizationEx = forall (v :: * -> *) (g :: *) (a :: *). OM v g a -> OM v g a


-- | map the graph optimization to each dataflow graph of the kernel
gmap :: (Graph v g a -> Graph v g a) -> OM v g a -> OM v g a
gmap f om = 
  om{ kernels = V.map (\k -> k{ dataflow = f $ dataflow k}) $ kernels om}
  
