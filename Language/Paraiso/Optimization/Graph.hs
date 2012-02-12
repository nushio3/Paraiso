{-# LANGUAGE CPP,  FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, 
 RankNTypes, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | Basic definitions for optimization 

module Language.Paraiso.Optimization.Graph (
  Optimization, OptimizationEx, 
  OptimizationOf, Ready,
  gmap
  ) where

import qualified Algebra.Additive        as Additive
import qualified Algebra.Ring            as Ring
import qualified Data.Tensor.TypeLevel   as Tensor
import           Data.Typeable
import qualified Data.Vector             as V
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM
import           Language.Paraiso.Prelude


-- | (Ready v g) indicates that the pair (v, g) has all the instances 
--   for the full optimizations to be serviced.
class (Tensor.Vector v, 
       Num g,
       Ord g, 
       Ring.C g, 
       Show g,
       Typeable g,
       Additive.C (v g), 
       Ord (v g), 
       Show (v g)) 
      => Ready (v :: * -> *) (g :: *)

instance (Tensor.Vector v, 
          Num g,
          Ord g, 
          Ring.C g, 
          Show g,
          Typeable g,
          Additive.C (v g), 
          Ord (v g), 
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
  
