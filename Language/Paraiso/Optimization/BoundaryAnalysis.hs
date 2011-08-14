{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Optimization.BoundaryAnalysis (
  boundaryAnalysis
  ) where

import qualified Algebra.Additive            as Additive
import qualified Data.Graph.Inductive        as FGL
import           Data.Maybe
import           Data.Typeable
import qualified Data.Vector                 as V
import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.Annotation.Boundary
import           Language.Paraiso.Interval
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.Realm as Realm
import           Language.Paraiso.PiSystem
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

-- | an optimization that changes nothing.
boundaryAnalysis :: (Vector v, Additive.C g, Ord g, Typeable g)
                    => Graph v g Anot.Annotation -> Graph v g Anot.Annotation
boundaryAnalysis graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = Anot.set (anot i) a

    anot i = memoAnot V.! i

    memoAnot = V.generate (FGL.noNodes graph) calcAnot

    calcAnot i = case selfNode of
      NValue v _           -> if isGlobal v 
                              then infinite
                              else preAnot
      NInst (Imm _)_       -> infinite
      NInst (Load _)_      -> full
      NInst (Store _)_     -> preAnot
      NInst (Reduce _)_    -> infinite
      NInst (Broadcast)_   -> infinite
      NInst (Shift v)_     -> full `intersection` shiftPreBy v 
      NInst (LoadIndex _)_ -> full
      NInst (Arith _)_     -> mergedAnot
      where
        self0 = FGL.lab graph i

        selfNode = case self0 of   
          Just x -> x
          _      -> error $ "node[" ++ show i ++ "] disappeared"

        isGlobal v = case v of
          DVal.DynValue Realm.Global _ -> True
          _                            -> False

        full = Valid $ toList $ fullValid graph
        infinite = Valid $ toList $ infiniteValid graph

        preAnot = case FGL.pre graph i of         
          [i'] -> anot i'
          xs    -> error $ "node[" ++ show i ++ "] only 1 pre expected : actually " ++ show (length xs)

        mergedAnot = case (FGL.pre graph i) of
          [] -> error $ "arith node[" ++ show i ++ "] has 0 pre"
          xs -> foldl1 intersection $ map anot $ xs

        shiftPreBy v = Valid $ zipWith shiftIntervalBy (toList v) ((\(Valid x)->x) preAnot)

        shiftIntervalBy x (Interval x1 x2) = Interval (add x x1) (add x x2)
        shiftIntervalBy _ Empty            = error "empty interval raised!"
        
        add x nby = case nby of
          NegaInfinity    -> NegaInfinity
          LowerBoundary y -> LowerBoundary $ x+y
          UpperBoundary y -> UpperBoundary $ x+y
          PosiInfinity    -> PosiInfinity

fullValid :: (Vector v, Additive.C g) => Graph v g a -> v (Interval (NearBoundary g))
fullValid _ = compose (\_ -> Interval (LowerBoundary Additive.zero) (UpperBoundary Additive.zero))

infiniteValid :: (Vector v, Additive.C g, Typeable g) => Graph v g a -> v (Interval (NearBoundary g))
infiniteValid _ = compose (\_ -> Interval NegaInfinity PosiInfinity)