{-# LANGUAGE  FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, 
NoImplicitPrelude, RankNTypes, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Optimization.Graph (
  Optimization, OptimizationEx, 
  OptimizationOf, Ready,
  gmap
  ) where

import qualified Algebra.Additive            as Additive
import           Data.Typeable
import qualified Data.Vector as V
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM
import           Language.Paraiso.Prelude
import qualified Language.Paraiso.Tensor as Tensor

-- | (Ready v g) indicates that the pair (v, g) has all the instances 
--   to receive full optimization services.
class (Tensor.Vector v, 
       Additive.C g, 
       Ord g, 
       Typeable g,
       Show (v g)) 
      => Ready (v :: * -> *) (g :: *)

instance (Tensor.Vector v, 
          Additive.C g, 
          Ord g, 
          Typeable g,
          Show (v g)) 
         => Ready (v :: * -> *) (g :: *)


-- | the most frequent type of optimization is which 
type Optimization   = forall (v :: * -> *) (g :: *). Graph v g Annotation -> Graph v g Annotation
type OptimizationEx = forall (v :: * -> *) (g :: *) (a :: *). OM v g a -> OM v g a
type OptimizationOf (v :: * -> *) (g :: *) = Graph v g Annotation -> Graph v g Annotation

-- | map the graph optimization to each dataflow graph of the kernel
gmap :: (Graph v g a -> Graph v g a) -> OM v g a -> OM v g a
gmap f om = 
  om{ kernels = V.map (\k -> k{ dataflow = f $ dataflow k}) $ kernels om}
  
