{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
module Language.Paraiso.Axis
    (
     (:.)(..), Axis(..),
     Vec(..), Vector(..),
     Vec1, Vec2, Vec3
    ) where

import Data.Array.Repa ((:.)(..))

data Axis = Axis Int deriving (Eq,Ord,Show,Read)

data Vec = Vec deriving (Eq,Ord,Show,Read)

type Vec1 a = Vec :. a
type Vec2 a = Vec1 a :. a
type Vec3 a = Vec2 a :. a

class Vector v a where
  getComponent :: Axis -> v a -> a
  dimension :: v a -> Int

instance Vector ((:.) Vec ) a where
  getComponent (Axis n) (Vec :. x) 
      | n == 0 = x
      | True   = error $ "axis out of bound"
  dimension _ = 1
  
instance (Vector v a) => Vector ((:.)(v a)) a where
  getComponent axis@(Axis n) (v :. x) 
      | n == dimension (v :. x) - 1 = x
      | True                        = getComponent axis v 
  dimension (v :. _) = 1 + dimension v


class  (Vector v a, Num a) => VectorNum v a where
    unitVector :: Axis -> v a

instance (Num a) => VectorNum ((:.) Vec ) a where
    unitVector = undefined