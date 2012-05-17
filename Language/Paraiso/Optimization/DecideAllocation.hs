{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

-- | The choice of making each Orthotope Machine node Manifest or not
-- largely depends on the user or the automated tuning.  However, code
-- generators requires that nodes are Manifest at certain contexts.
--
-- decideAllocation makes sure that such nodes are marked as Manifest,
-- and also makes sure that every node is marked with at least some 
-- Allocation.

module Language.Paraiso.Optimization.DecideAllocation (
  decideAllocation
  ) where

import qualified Data.Graph.Inductive                   as FGL
import           Data.Maybe 
import qualified Language.Paraiso.Annotation            as Anot
import           Language.Paraiso.Annotation.Allocation as Alloc
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.DynValue           as DVal
import           Language.Paraiso.OM.Realm              as Realm
import           Language.Paraiso.Optimization.Graph

decideAllocation :: Optimization
decideAllocation graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i  
      | afterLoad = Anot.set Alloc.Existing
      | beforeStore || beforeReduce || afterReduce || beforeBroadcast || afterBroadcast
                  = Anot.set Alloc.Manifest
      | (isScalar || beforeShift || afterShift ) && False -- warehouse
                  = Anot.weakSet Alloc.Delayed
      | otherwise = Anot.weakSet Alloc.Delayed . setChoice
        where
          self0 = FGL.lab graph i
          pre0  = FGL.lab graph =<<(listToMaybe $ FGL.pre graph i) 
          sucs  = catMaybes $ map (FGL.lab graph) $ FGL.suc graph i

          setChoice 
            | isValue   = Anot.set $ Alloc.AllocationChoice [Alloc.Delayed, Alloc.Manifest]
            | otherwise = id

          isValue  = case self0 of
            Just (NValue _ _) -> True
            _                 -> False

          isScalar  = case self0 of
            Just (NValue (DVal.DynValue Realm.Scalar _) _) -> True
            _                                              -> False
          afterLoad = case pre0 of
            Just (NInst (Load _) _)    -> True
            _                          -> False
          beforeStore  = 
            or $
            flip map sucs $ \ nd -> case nd of
              (NInst (Store _) _)   -> True
              _                     -> False
          beforeReduce = 
            or $
            flip map sucs $ \ nd -> case nd of
              (NInst (Reduce _) _)  -> True
              _                     -> False
          afterReduce = case pre0 of
            Just (NInst (Reduce _) _)  -> True
            _                          -> False
          beforeBroadcast =
            or $
            flip map sucs $ \ nd -> case nd of
              (NInst (Broadcast) _) -> True
              _                     -> False
          afterBroadcast = case pre0 of
            Just (NInst (Broadcast) _) -> True
            _                          -> False
          beforeShift = 
            or $
            flip map sucs $ \ nd -> case nd of
              (NInst (Shift _) _)   -> True
              _                     -> False
          afterShift = case pre0 of
            Just (NInst (Shift _) _)   -> True
            _                          -> False




