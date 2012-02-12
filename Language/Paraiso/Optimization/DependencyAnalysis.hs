{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

-- | This module performs dependency analysis for generating subroutines. 
--
-- 1. Direct dependency between Manifest/Existing nodes X and Y: the
-- subroutine that outputs Y needs to read X as input
--
-- 2. Indirect dependency between Manifest nodes X and Y: the
-- computation of Y requires that the computation of X is finished,
-- and therefore X and Y cannot be output by the same subroutine
--
-- 3. Calculation dependency between any node X and Manifest node Y:
-- in the subroutine you output Y you need to calculate X.
--
-- c.f. 'Language.Paraiso.Annotation.Dependency' 

module Language.Paraiso.Optimization.DependencyAnalysis (
  writeGrouping
  ) where

import qualified Data.Graph.Inductive                   as FGL
import qualified Data.Set                               as Set
import qualified Data.Vector                            as V
import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import qualified Language.Paraiso.Annotation.Boundary   as Boundary
import qualified Language.Paraiso.Annotation.Dependency as Depend
import           Language.Paraiso.OM
import qualified Language.Paraiso.OM.DynValue           as DVal
import           Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm              as Realm
import qualified Language.Paraiso.Optimization.Graph    as Opt

-- | Give unique numbering to each groups in the entire OM 
--   in preparation for code generation
writeGrouping :: Opt.Ready v g => OM v g Anot.Annotation -> OM v g Anot.Annotation
writeGrouping om0 = om { kernels = kernelsRet}
  where
    om = Opt.gmap dependencyAnalysis om0
    kernels0 = kernels om
    graphs0 = V.map dataflow $ kernels0
    graphsSize = V.length graphs0
    
    -- | how many kernel groups are included in i'th graph?
    groupCount = flip V.map graphs0
      (\graph -> (1+) $ maximum $ (-1 : ) $ concat $
                 map (map Depend.getKernelGroupID . a2k . getA) $ 
                 map snd $ FGL.labNodes graph)
    
    a2k :: Anot.Annotation -> [Depend.KernelWriteGroup]
    a2k a = case Anot.toMaybe a of
      Just kwg -> [kwg]
      Nothing  -> [] -- non-manifest nodes does not contain Group
        
    graphsRet = V.generate graphsSize (\i -> renumber i $ graphs0 V.! i)
 
    kernelsRet = flip V.imap kernels0
      (\i kern -> kern { dataflow = graphsRet V.! i})
    
    renumber idx graph = 
      let diff = V.sum$ V.take idx groupCount in
      flip nmap graph $
        Anot.map (Depend.OMWriteGroup . (diff+) . Depend.getKernelGroupID)
      
-- | dependency analysis
dependencyAnalysis :: Opt.Ready v g => Opt.OptimizationOf v g
dependencyAnalysis graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i = setGroup i . (Anot.set $ calcList i) . (Anot.set $ dependencyList i) . (Anot.set $ indirectList i) 
    
    dependencyList idx = -- a node wite index idx
      Depend.Direct $    -- depends directly
      V.toList $
      V.map fst $        -- on the nodes with jdx where
      V.filter snd $     -- 'True' is written at
      V.imap (\sidx dep -> (sidxToIdx V.! sidx, dep)) $
      dependMatrixWrite V.! idx -- dependMatrixWrite ! idx ! sidx
      -- where  staticIndexs jdx = sidx in
      
    indirectList idx =  -- a node wite index idx
      Depend.Indirect $ -- depends indirectly
      V.toList $
      V.map fst $       -- on the nodes with jdx where
      V.filter snd $    -- 'True' is written at
      V.imap (\sidx dep -> (sidxToIdx V.! sidx, dep)) $
      indirectMatrixWrite V.! idx -- dependMatrixWrite ! idx ! sidx
      -- where  staticIndexs jdx = sidx in

    calcList idx = -- a set of node idxs needed within subroutine that calculate node idx
      Depend.Calc $
      calcMatrixWrite V.! idx 


    setGroup idx = 
      case idxToAlloc V.! idx of
        Alloc.Manifest -> Anot.set $ Depend.KernelWriteGroup (kernelGroup V.! idx)
        _              -> id
          
    -- Number of Strict Nodes
    sidxSize = V.length sidxToIdx
    -- Number of all nodes
    idxSize  = FGL.noNodes graph


    -- group to which a Manifest node belongs
    kernelGroup :: V.Vector Int
    kernelGroup = V.generate idxSize inner
      where
        inner idx 
          -- if there is no predecessor you are the first
          | length pres == 0        = 0
          -- you found someone else you can coexist with
          | not (null coGroups) = head coGroups
          -- there are predecessors, but you can't coexist with any of them
          | otherwise               = 1 + maximum (map (kernelGroup V.!) pres)
          where
            pres = takeWhile (<idx) manifestNodes
            existingGroups = Set.toList $ Set.fromList $ map (kernelGroup V.!) pres
            groupMember grp = filter ((==grp) . (kernelGroup V.!)) pres
            coGroups = filter (and . map (coexist idx) .  groupMember) existingGroups
    
    -- whether two nodes can be written simultaneously in one kernel.
    coexist :: FGL.Node -> FGL.Node -> Bool
    coexist idx jdx
      | (idxToAlloc V.! idx /= Alloc.Manifest || idxToAlloc V.! jdx /= Alloc.Manifest )
                   = error "coexistence not defined for non-Manifest nodes"
      | idx == jdx = True
      | idx <  jdx = coexist jdx idx
      | otherwise  = (not dependent') && sameShape
      where
        dependent' = indirectMatrixWrite V.! idx V.! (idxToSidx V.! jdx)
        sameShape  = 
          (idxToRealm V.! idx) == (idxToRealm V.! jdx) &&
          (idxToValid V.! idx) == (idxToValid V.! jdx)
        
    -- the allocation setting of each node.
    idxToAlloc :: V.Vector Alloc.Allocation
    idxToAlloc = V.fromList $
      map (\(_, nd) -> f' $ Anot.toMaybe $ getA nd) $
      FGL.labNodes graph
      where
        f' (Just x) = x
        f' Nothing  = error "writeGrouping must be done after decideAllocation"

    -- the allocation setting of each node.
    -- the gauge for validness should copy the gauge of the graph.
    idxToValid = idxToValid' graph
    idxToValid' :: (Opt.Ready v g) => (Graph v g Anot.Annotation) -> V.Vector (Boundary.Valid g)
    idxToValid' _  = V.fromList $
      map (\(_, nd) -> f' $ Anot.toMaybe $ getA nd) $
      FGL.labNodes graph
      where
        f' (Just x) = x
        f' Nothing  = error "boundaryAnalysis must be done after decideAllocation"

    -- the OM node for each index
    idxToRealm :: V.Vector Realm.Realm
    idxToRealm = V.generate idxSize inner
      where
        inner idx = case FGL.lab graph idx of
          Just (NValue (DVal.DynValue r _)_) -> r
          Just _                             -> error "realm required for non-Value node"
          Nothing                            -> error "indexing mismatch"

    -- the list of indices of Manifest nodes in ascending order.
    manifestNodes :: [FGL.Node]
    manifestNodes = 
      map fst $
      filter snd $
      map (\(idx, nd) -> (idx, (==Just Alloc.Manifest) $ Anot.toMaybe $ getA nd)) $
      FGL.labNodes graph

    -- map index of a StrictNode to the FGL node index 
    sidxToIdx :: V.Vector Int
    sidxToIdx = 
      V.map fst $ 
      V.filter snd $ 
      V.imap (\idx isStrict' -> (idx, isStrict')) $ 
      isStrict 
    -- map FGL node index to StrictNode index
    idxToSidx :: V.Vector Int
    idxToSidx = V.generate idxSize inner
      where
        inner idx =
          head $
          (++ [error $ show idx ++ " is not a Strict node"]) $
          V.toList $
          V.map fst $
          V.filter ((==idx) . snd) $
          V.imap (\sidx' idx' -> (sidx', idx')) $
          sidxToIdx

    isStrict :: V.Vector Bool
    isStrict = V.generate idxSize $ \idx ->
      case idxToAlloc V.! idx of
        Alloc.Manifest -> True
        Alloc.Existing -> True
        Alloc.Delayed  -> False

    -- (dependMatrix ! idx) = the list of sidx that is needed to create the value for this idx
    dependMatrixWrite :: V.Vector (V.Vector Bool)
    dependMatrixWrite = V.generate idxSize dependRowWrite

    -- (dependMatrix ! idx) = the list of sidx that is needed to refer to the idx
    dependMatrixRead :: V.Vector (V.Vector Bool)
    dependMatrixRead = V.generate idxSize dependRowRead

    -- everyone depends on its predecessors
    dependRowWrite idx = foldl mergeRow allFalseRow $ map (dependMatrixRead V.!) $ FGL.pre graph idx

    dependRowRead idx 
      -- a strict node can be read by itself
      | isStrict V.! idx = V.map (==idx) sidxToIdx 
      -- a non-manifest node depends on its predecessors
      | otherwise          = dependMatrixWrite V.! idx

    -- (indirectMatrix ! idx) = the list of sidx that is needed to create the value for this idx
    indirectMatrixWrite :: V.Vector (V.Vector Bool)
    indirectMatrixWrite = V.generate idxSize indirectRowWrite

    -- (indirectMatrix ! idx) = the list of sidx that is needed to refer to the idx
    indirectMatrixRead :: V.Vector (V.Vector Bool)
    indirectMatrixRead = V.generate idxSize indirectRowRead

    -- everyone indirects on its predecessors
    indirectRowWrite idx = foldl mergeRow allFalseRow $ map (indirectMatrixRead V.!) $ FGL.pre graph idx

    indirectRowRead idx 
      -- add a strict node read to indirect dependency
      | isStrict V.! idx = (V.map (==idx) sidxToIdx) `mergeRow` (indirectMatrixWrite V.! idx)
      -- a non-manifest node indirects on its predecessors
      | otherwise          = indirectMatrixWrite V.! idx


    allFalseRow = V.replicate sidxSize False
    mergeRow va vb
      | V.length va /= sidxSize = error "wrong size contamination in dependMatrix"
      | V.length vb /= sidxSize = error "wrong size contamination in dependMatrix" 
      | otherwise               = V.zipWith (||) va vb
    


    -- The every noe touched in the kernel to calculate the node
    calcMatrixRead :: V.Vector (Set.Set FGL.Node)
    calcMatrixRead = V.generate idxSize calcRowRead
    
    calcMatrixWrite :: V.Vector (Set.Set FGL.Node)
    calcMatrixWrite = V.generate idxSize calcRowWrite

    calcRowWrite idx = Set.unions $ map (calcMatrixRead V.!) $ FGL.pre graph idx
    
    calcRowRead idx
      -- strict nodes depends only on itself
      | isStrict V.! idx = Set.fromList [idx]
      -- a delayed node indirects on its predecessors
      | otherwise        = Set.union (Set.fromList [idx]) $ calcMatrixWrite V.! idx
