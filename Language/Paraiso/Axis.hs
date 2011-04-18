{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

{- | This module defines 'Vector' class, whose components can be specified by
   'Axis'. a multi-dimensional vector constructor ':.' is shared with
   Data.Array.Repa.  -}

module Language.Paraiso.Axis
    (
     (:.)(..), Axis(..),
     Vec(..), Vector(..), VectorNum(..),
     Vec1, Vec2, Vec3
    ) where

import Control.Applicative
import Data.Foldable
import Data.Array.Repa ((:.)(..))
import Data.Traversable


-- | An coordinate 'Axis' , labeled by an integer. 
data Axis = Axis Int deriving (Eq,Ord,Show,Read)

-- | A 'Vector' is tuple of type a, whose components can be accessed by Axes.
class Vector v a where
  getComponent :: Axis -> v a -> a
  dimension :: v a -> Int

-- | 'Vec' is a vector in linear algebraic sense, of arbitrary dimension.
data Vec = Vec deriving (Eq,Ord,Show,Read)

-- | Type synonims for 'Vec's.
type Vec1 a = Vec :. a
type Vec2 a = Vec1 a :. a
type Vec3 a = Vec2 a :. a

-- | One-dimensional 'Vec' is an instance of 'Vector'.
instance Vector ((:.) Vec) a where
  getComponent (Axis n) (Vec :. x) 
      | n == 0 = x
      | True   = error "axis out of bound"
  dimension _ = 1
  
-- | if n-dimensional 'Vec' is an instance of 'Vector', (n+1)-dimensional 'Vec' is also 'Vector'.
instance (Vector v a) => Vector ((:.)(v a)) a where
  getComponent axis@(Axis n) (v :. x) 
      | n == dimension (v :. x) - 1 = x
      | True                        = getComponent axis v 
  dimension (v :. _) = 1 + dimension v

-- | 'VectorNum' is a 'Vector' whose components are of instance 'Num'.
class  (Vector v a, Num a) => VectorNum v a where
  -- | A vector whose components are all zero.
  zeroVector :: v a 

  -- | A vector where 'Axis'th component is unity but others are zero.
  unitVector :: Axis -> v a 
    
-- | One-dimensional 'Vec' is an instance of 'VectorNum'.
instance (Num a) => VectorNum ((:.) Vec) a where
    zeroVector = Vec :. 0  
    unitVector (Axis n)
        | n == 0 = Vec :. 1
        | True   = error "axis out of bound"

-- | if n-dimensional 'Vec' is an instance of 'VectorNum', (n+1)-dimensional 'Vec' is also 'VectorNum'.
instance (VectorNum v a) => VectorNum ((:.)(v a)) a where
    zeroVector = zeroVector :. 0  
    unitVector axis@(Axis n) = ret
        where
          z = zeroVector
          d = dimension z
          ret 
              | n < 0 || n >= d   = error "axis out of bound"
              | n == d-1          = zeroVector :. 1
              | 0 <= n && n < d-1 = unitVector axis :. 0
              | True              = z -- needed to infer type of z


-- One-dimensional 'Vec' is an instance of 'Traversable'.
instance Foldable ((:.) Vec) where
  foldMap = foldMapDefault
instance Functor ((:.) Vec) where
  fmap = fmapDefault
instance Traversable ((:.) Vec) where
  traverse f (Vec :. x) = (Vec:.) <$> f x

-- if n-dimensional 'Vec' is an instance of 'Traversable', (n+1)-dimensional 'Vec' is also 'Traversable'.
{-
instance (Traversable v, Vector v a) => Foldable ((:.) (v a)) where
  foldMap = foldMapDefault
instance (Traversable v, Vector v a) => Functor ((:.) (v a)) where
  fmap = fmapDefault
instance (Traversable v, Vector v a) => Traversable (\a -> (:.) (v a) a) where
  traverse f (v :. x) = (:.) <$> traverse f v <*> f x
-}

instance Foldable Vec3 where
  foldMap = foldMapDefault
instance Functor Vec3 where
  fmap = fmapDefault
instance Traversable Vec3 where
  traverse f (Vec :. x :. y :. z) = (\fx fy fz -> Vec:.fx:.fy:fz) <$> f x <*> f y <*> f z

