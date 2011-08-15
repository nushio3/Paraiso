{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.Plan (
  Plan(..),
  
  SubKernelRef(..), StorageRef(..),
  
  dataflow, labNodesIn, labNodesOut,
  storageType                  
  ) where

import qualified Data.Graph.Inductive as FGL
import qualified Data.Vector as V
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM.DynValue as DVal
import qualified Language.Paraiso.OM.Graph as OM
import           Language.Paraiso.Prelude


-- | data 
data Plan v g a
  = Plan 
    { planName   :: Name,
      setup      :: OM.Setup v g a,
      kernels    :: V.Vector (OM.Kernel v g a),
      storages   :: V.Vector (StorageRef v g a),
      subKernels :: V.Vector (SubKernelRef v g a)
    }

instance Nameable (Plan v g a) where name = planName    
                                     
class Referrer a b | a->b where
  parent :: a -> b

-- | subroutines that executes portion of a certain kernel
data SubKernelRef v g a
  = SubKernelRef
    { subKernelParen :: Plan v g a,
      kernelIdx  :: Int,
      inputIdxs  :: V.Vector FGL.Node,
      calcIdxs   :: V.Vector FGL.Node,
      outputIdxs :: V.Vector FGL.Node
    }
    
instance Referrer (SubKernelRef v g a) (Plan v g a) where
  parent = subKernelParen
 
-- | refers to the storage required in the plan
data StorageRef v g a
  = StaticRef (Plan v g a) Int -- ^ (StatigRef plan i) = i'th static variable in the plan
  | ManifestRef (Plan v g a) Int FGL.Node -- ^ (ManifestRef plan i j) = j'th node of the i'th kernel in the plan

instance Referrer (StorageRef v g a) (Plan v g a) where
  parent (StaticRef p _)      = p
  parent (ManifestRef p _ _ ) = p


dataflow :: SubKernelRef v g a -> OM.Graph v g a
dataflow ref = OM.dataflow $ (kernels $ parent ref) V.! (kernelIdx ref)
    
labNodesIn :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesIn ref = 
  flip V.map (inputIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"
                   
labNodesCalc :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesCalc ref = 
  flip V.map (calcIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"


labNodesOut :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesOut ref = 
  flip V.map (outputIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"
  
-- | get the DynValue description for current storage referrence.  
storageType :: StorageRef v g a -> DVal.DynValue
storageType (StaticRef p i) = namee $ (OM.staticValues $ setup p) V.! i
storageType (ManifestRef p i j) = case FGL.lab (OM.dataflow $ kernels p V.! i) j of
  Just (OM.NValue x _) -> x
  Just _               -> error $ "node [" ++ show j ++ "] in kernel [" ++ show i ++ "] is not a Value node" 
  Nothing              -> error $ "node [" ++ show j ++ "] does not exist in kernel [" ++ show i ++ "]"

