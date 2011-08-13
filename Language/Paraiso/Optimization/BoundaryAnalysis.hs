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
import           Language.Paraiso.Optimization.Graph
import           Language.Paraiso.PiSystem
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

-- | an optimization that changes nothing.
boundaryAnalysis :: (Vector v, Additive.C g, Ord g, Typeable (v (Interval (NearBoundary g))))
                    => Graph v g Anot.Annotation -> Graph v g Anot.Annotation
boundaryAnalysis graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = Anot.set (Valid $ anot i) a

    anot i = memoAnot V.! i

    memoAnot = V.generate (FGL.noNodes graph) calcAnot

    calcAnot i = case selfNode of
      NValue v _            -> if isGlobal v 
                               then infiniteValid graph 
                               else anot (uniquePre i)
      NInst (Imm _) _       -> infiniteValid graph
      NInst (Load _) _      -> fullValid graph
      NInst (Store _) _     -> anot (uniquePre i)
      NInst (Reduce _) _    -> infiniteValid graph
      NInst (Broadcast) _   -> infiniteValid graph
      NInst (Shift _) _     -> fullValid graph `intersection` anot (uniquePre i)
      NInst (LoadIndex _) _ -> anot (uniquePre i)
      NInst (Arith _) _     -> merge i
      _                     -> error "unsupported path in boundary analysis"
      where
        self0 = FGL.lab graph i
        pre0  = FGL.lab graph =<<(listToMaybe $ FGL.pre graph i) 
        suc0  = FGL.lab graph =<<(listToMaybe $ FGL.suc graph i) 

        selfNode = case self0 of   
          Just x -> x
          _      -> error $ "node[" ++ show i ++ "] disappeared"

        uniquePre i = case FGL.pre graph i of         
          [i'] -> i'
          xs    -> error $ "node[" ++ show i ++ "] only 1 pre expected : actually " ++ show (length xs)

        isGlobal v = case v of
          DVal.DynValue Realm.Global _ -> True
          _                            -> False

        merge i = case (FGL.pre graph i) of
          [] -> error $ "arith node[" ++ show i ++ "] has 0 pre"
          xs -> foldl1 intersection $ map anot $ xs

fullValid :: (Vector v, Additive.C g) => Graph v g a -> v (Interval (NearBoundary g))
fullValid _ = compose (\_ -> Interval (LowerBoundary Additive.zero) (UpperBoundary Additive.zero))

infiniteValid :: (Vector v, Additive.C g) => Graph v g a -> v (Interval (NearBoundary g))
infiniteValid _ = compose (\_ -> Interval NegaInfinity PosiInfinity)