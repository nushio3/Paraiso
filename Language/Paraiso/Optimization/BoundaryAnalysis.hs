{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | The volume of mesh that contains the correct results shrinks as
-- the stencil calculation proceeds. This is because in stencil
-- calculation each mesh access to its neighbour meshes and for
-- boundary meshes it cannot be obtained.
--
-- boundaryAnalysis marks each node of the Orthotope Machine with the
-- region for which the computation result is Valid.

module Language.Paraiso.Optimization.BoundaryAnalysis (
  boundaryAnalysis
  ) where

import qualified Algebra.Additive            as Additive
import qualified Data.Graph.Inductive        as FGL
import           Data.Foldable (toList)
import           Data.Maybe
import           Data.Tensor.TypeLevel
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
import           NumericPrelude hiding ((++))

boundaryAnalysis :: (Vector v, Additive.C g, Ord g, Typeable g)
                    => Graph v g Anot.Annotation -> Graph v g Anot.Annotation
boundaryAnalysis graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = Anot.set (anot i) a

    anot i = memoAnot V.! i

    memoAnot = V.generate (FGL.noNodes graph) calcAnot

    calcAnot i = case selfNode of
      NValue v _           -> if isScalar v 
                              then infinite
                              else preAnot
      NInst (Imm _)_       -> infinite
      NInst (Load _)_      -> full
      NInst (Store _)_     -> preAnot
      NInst (Reduce _)_    -> infinite
      NInst (Broadcast)_   -> infinite
      NInst (Shift v)_     -> full `intersection` shiftPreBy v 
      NInst (LoadIndex _)_ -> infinite
      NInst (LoadSize _)_  -> infinite
      NInst (Arith _)_     -> mergedAnot
      where
        self0 = FGL.lab graph i

        selfNode = case self0 of   
          Just x -> x
          _      -> error $ "node[" ++ show i ++ "] disappeared"

        isScalar v = case v of
          DVal.DynValue Realm.Scalar _ -> True
          _                            -> False

        -- data at all coordinates is valid that is stored in the array 
        full = Valid $ toList $ fullValid graph
        -- data at arbitrary coordinates is valid including imaginary
        -- coordinates beyond the range of the array
        infinite = Valid $ toList $ infiniteValid graph

        -- the Valid region of the single preceding value
        preAnot = case FGL.pre graph i of         
          [i'] -> anot i'
          xs    -> error $ "node[" ++ show i ++ "] only 1 pre expected : actually " ++ show (length xs)

        -- intersection of the Valid regions of preceding nodes
        mergedAnot = case (FGL.pre graph i) of
          [] -> error $ "arith node[" ++ show i ++ "] has 0 pre"
          xs -> foldl1 intersection $ map anot $ xs

        --  shift the preceding Valid by a vector v
        shiftPreBy v = Valid $ 
            zipWith shiftIntervalBy (toList v) ((\(Valid x)->x) preAnot)
        shiftIntervalBy x (Interval x1 x2) = Interval (add x x1) (add x x2)
        shiftIntervalBy _ Empty            = error "empty interval raised!"
        
        add x nby = case nby of
          NegaInfinity    -> NegaInfinity
          LowerBoundary y -> LowerBoundary $ x+y
          UpperBoundary y -> UpperBoundary $ x+y
          PosiInfinity    -> PosiInfinity

-- | data at all coordinates is valid that is stored in the array 
fullValid :: (Vector v, Additive.C g) => Graph v g a -> v (Interval (NearBoundary g))
fullValid _ = compose (\_ -> Interval (LowerBoundary Additive.zero) (UpperBoundary Additive.zero))

-- | data at arbitrary coordinates is valid including imaginary
-- coordinates beyond the range of the array
infiniteValid :: (Vector v, Additive.C g, Typeable g) => Graph v g a -> v (Interval (NearBoundary g))
infiniteValid _ = compose (\_ -> Interval NegaInfinity PosiInfinity)