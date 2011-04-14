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
  getComponent :: v a -> Axis -> a

instance Vector ((:.) Vec ) a where
  getComponent (Vec :. x) axis = x
  
instance (Vector v a) => Vector ((:.)(v a)) a where
  getComponent (_ :. x) axis = x