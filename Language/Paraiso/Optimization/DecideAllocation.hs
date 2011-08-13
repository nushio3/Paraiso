{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Optimization.DecideAllocation (
  decideAllocation
  ) where

import qualified Data.Graph.Inductive                   as FGL
import           Data.Maybe 
import qualified Language.Paraiso.Annotation            as Anot
import           Language.Paraiso.Annotation.Allocation as Alloc
import           Language.Paraiso.Prelude
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.DynValue           as DVal
import           Language.Paraiso.OM.Realm              as Realm
import           Language.Paraiso.Optimization.Graph

decideAllocation :: Optimization
decideAllocation graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = Anot.set (anot i) a

    anot i =
      if isGlobal || afterLoad || beforeStore 
         || beforeReduce || afterReduce 
         || (False &&( beforeShift && afterShift))
      then Alloc.Manifest
      else Alloc.Delayed
        where
          self0 = FGL.lab graph i
          pre0  = FGL.lab graph =<<(listToMaybe $ FGL.pre graph i) 
          suc0  = FGL.lab graph =<<(listToMaybe $ FGL.suc graph i) 
          isGlobal  = case self0 of
                        Just (NValue (DVal.DynValue Realm.Global _) _) -> True
                        _                                              -> False
          afterLoad = case pre0 of
                        Just (NInst (Load _) _) -> True
                        _                       -> False
          beforeStore   = case suc0 of
                        Just (NInst (Store _) _) -> True
                        _                        -> False
          beforeReduce = case suc0 of
                        Just (NInst (Reduce _) _) -> True
                        _                         -> False
          afterReduce = case pre0 of
                        Just (NInst (Reduce _) _) -> True
                        _                         -> False

          beforeShift = case suc0 of
                        Just (NInst (Shift _) _) -> True
                        _                         -> False
          afterShift = case pre0 of
                        Just (NInst (Shift _) _) -> True
                        _                         -> False




