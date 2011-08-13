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


-- | The dimension of the vector space the axis belongs to.
dimension :: (Vector v) => Axis v -> Int
dimension axis = T.dimension $ compose (\axis' -> [axis, axis'])

-- | The next axis under the Law of Cycles.
next :: (Vector v) => Axis v -> Axis v
next axis = Axis $ (axisIndex axis + 1) `mod` dimension axis

-- | The previous axis under the Law of Cycles.
prev :: (Vector v) => Axis v -> Axis v
prev axis = Axis $ (axisIndex axis - 1) `mod` dimension axis


-- | All the axes belonging to the dimension.
all :: (Vector v) => Axis v -> [Axis v]
all axis = let dim = dimension axis in
              map head [[Axis i, axis] | i<-[0..dim-1]]
-- | All the axes belonging to the dimension,               
-- starting from the argument and followed by the Law of Cycles.
allFrom :: (Vector v) => Axis v -> [Axis v]
allFrom axis = let dim = dimension axis in
              map head [[Axis $ (axisIndex axis+i) `mod` dim, axis] | i<-[0..dim-1]]              
-- | All the axes belonging to the dimension but the argument itself,               
-- in the order of the Law of Cycles.              
others :: (Vector v) => Axis v -> [Axis v]
others axis = let dim = dimension axis in
              map head [[Axis $ (axisIndex axis+i) `mod` dim, axis] | i<-[1..dim-1]]
              