{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | Axis utility functions.
-- use this like 
-- > import qualified Language.Paraiso.Axis as Axis
module Language.Paraiso.Axis
       (dimension, next, prev, all, allFrom, others)
    where

import           Language.Paraiso.Prelude hiding (all)
import           Language.Paraiso.Tensor hiding (dimension)
import qualified Language.Paraiso.Tensor as T (dimension)

dimension :: (Vector v) => Axis v -> Int
dimension axis = T.dimension $ compose (\axis' -> [axis, axis'])

next :: (Vector v) => Axis v -> Axis v
next axis = Axis $ (axisIndex axis + 1) `mod` dimension axis

prev :: (Vector v) => Axis v -> Axis v
prev axis = Axis $ (axisIndex axis - 1) `mod` dimension axis

all, allFrom, others :: (Vector v) => Axis v -> [Axis v]

all    axis = let dim = dimension axis in
              map head [[Axis i, axis] | i<-[0..dim-1]]
allFrom axis = let dim = dimension axis in
              map head [[Axis $ (axisIndex axis+i) `mod` dim, axis] | i<-[0..dim-1]]              
others  axis = let dim = dimension axis in
              map head [[Axis $ (axisIndex axis+i) `mod` dim, axis] | i<-[1..dim-1]]
              