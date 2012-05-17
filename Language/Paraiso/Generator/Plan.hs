{-# LANGUAGE CPP, FunctionalDependencies, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | Taking the optimized OM as the input,
-- The 'Plan' summarizes and fixes the detail of the code generation,
-- such as amount of memory to be allocated,
-- the extra subroutines which does internal calculations,
-- and decisions on which part of calculation each subroutines make
-- etc.

module Language.Paraiso.Generator.Plan (
  Plan(..),

  SubKernelRef(..), StorageRef(..), StorageIdx(..), Referrer(..),

  dataflow, labNodesIn, labNodesOut, labNodesCalc, 
  storageType                  
  ) where

import qualified Data.Graph.Inductive as FGL
import qualified Data.Vector as V
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM.DynValue as DVal
import qualified Language.Paraiso.OM.Graph as OM
import qualified Language.Paraiso.OM.Realm as Realm
import           Language.Paraiso.Prelude
import           NumericPrelude hiding ((++))


-- | A data structure that contains all informations
-- for code generation.
data Plan v g a
  = Plan 
    { planName   :: Name, -- ^ name of the plan
      setup      :: OM.Setup v g a, -- ^ OM setup, includes all static variables
      storages   :: V.Vector (StorageRef v g a), -- ^ Newly allocated Manifest variables
      kernels    :: V.Vector (OM.Kernel v g a), -- ^ kernels
      subKernels :: V.Vector (SubKernelRef v g a), -- ^ subkernels
      lowerMargin :: v g, -- ^ the total lower margin of the OM, large enough to fit all the kernels
      upperMargin :: v g  -- ^ the total upper margin of the OM, large enough to fit all the kernels
    }

instance Nameable (Plan v g a) where name = planName    

-- | a data that holds referrence to the Plan it belongs to.
class Referrer a b | a->b where
  parent :: a -> b

-- | subroutines that executes portion of a calculations for certain kernel
data SubKernelRef v g a
  = SubKernelRef
    { subKernelParent :: Plan v g a,
      kernelIdx       :: Int,
      omWriteGroupIdx :: Int,      
      inputIdxs       :: V.Vector FGL.Node,
      calcIdxs        :: V.Vector FGL.Node,
      outputIdxs      :: V.Vector FGL.Node,
      subKernelRealm  :: Realm.Realm,
      lowerBoundary   :: v g,
      upperBoundary   :: v g
    }

instance Referrer (SubKernelRef v g a) (Plan v g a) where
  parent = subKernelParent
instance Nameable (SubKernelRef v g a) where
  name x = let
    na = nameText $ kernels (parent x) V.! (kernelIdx x)
    in mkName $ "om_" ++ na ++ "_sub_" ++ showT (omWriteGroupIdx x)
instance Realm.Realmable (SubKernelRef v g a) where
  realm = subKernelRealm

-- | refers to a storage required in the plan
data StorageRef v g a
  = StorageRef
  { storageRefParent :: Plan v g a,
    storageIdx       :: StorageIdx,
    storageDynValue  :: DVal.DynValue
  }

data StorageIdx 
  = StaticRef   Int -- ^ (StatigRef plan i) = i'th static variable in the plan
  | ManifestRef Int FGL.Node -- ^ (ManifestRef plan i j) = j'th node of the i'th kernel in the plan
  deriving (Eq, Show)

instance Referrer (StorageRef v g a) (Plan v g a) where
  parent = storageRefParent
instance Nameable (StorageRef v g a) where
  name x = mkName $ case storageIdx x of
    StaticRef i     -> "om_s" ++ showT i ++ "_"
                       ++ nameText (OM.staticValues (setup $ parent x) V.! i)
    ManifestRef i j -> "om_m" ++ showT i ++ "_" ++ showT j
    
    
instance Realm.Realmable (StorageRef v g a) where
  realm x = let DVal.DynValue r _ = storageDynValue x in r

dataflow :: SubKernelRef v g a -> OM.Graph v g a
dataflow ref = OM.dataflow $ (kernels $ parent ref) V.! (kernelIdx ref)

-- | a list of inputs the subroutine needs.
labNodesIn :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesIn ref = 
  flip V.map (inputIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"

-- | all the caclulations performed in the subroutine.
labNodesCalc :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesCalc ref = 
  flip V.map (calcIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"

-- | a list of outputs the subroutine makes.
labNodesOut :: SubKernelRef v g a -> V.Vector(FGL.LNode (OM.Node v g a))
labNodesOut ref = 
  flip V.map (outputIdxs ref) $
  \idx -> case FGL.lab (dataflow ref) idx of
    Just x  -> (idx, x)
    Nothing -> error $ "node [" ++ show idx ++ "] does not exist in kernel [" ++ show (kernelIdx ref) ++ "]"

-- | get the DynValue description for a storage referrence.  
storageType :: StorageRef v g a -> DVal.DynValue
storageType 
  StorageRef {
    storageRefParent = p,
    storageIdx       = StaticRef i
    } = namee $ (OM.staticValues $ setup p) V.! i
storageType 
  StorageRef {
    storageRefParent = p,
    storageIdx       = ManifestRef i j
    } = case FGL.lab (OM.dataflow $ kernels p V.! i) j of
    Just (OM.NValue x _) -> x
    Just _               -> error $ "node [" ++ show j ++ "] in kernel [" ++ show i ++ "] is not a Value node" 
    Nothing              -> error $ "node [" ++ show j ++ "] does not exist in kernel [" ++ show i ++ "]"

