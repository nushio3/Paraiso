{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, NoImplicitPrelude, StandaloneDeriving, 
  TypeOperators #-} 
{-# OPTIONS -Wall #-}
-- | A tensor algebra library. Main ingredients are :
-- 
-- 'Vec' and ':~' are data constructors for rank-1 tensor.
-- This is essentially a touple of objects of the same type.
-- 
-- 'Vector' is a class for rank-1 tensor.
--
-- 'Axis' is an object for accessing the tensor components.

module Language.Paraiso.Tensor
    (
     (:~)(..), Vec(..), Axis(..), 
     Vector(..), VectorAdditive(..), VectorRing(..),
     Vec0, Vec1, Vec2, Vec3, Vec4
    ) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Control.Applicative
import Control.Monad.Failure
import Data.Foldable
import Data.Traversable
import NumericPrelude
import System.IO.Unsafe


import Control.Monad
unsafePerformFailure :: IO a -> a
unsafePerformFailure = unsafePerformIO

-- | data constructor for 0-dimensional tensor.
data Vec a = Vec 
infixl 3 :~
-- | data constructor for constructing n+1-dimensional tensor
-- from n-dimensional tensor.
data n :~ a = (n a) :~ a

deriving instance (Show a) => Show (Vec a)
deriving instance (Show a, Show (n a)) => Show (n :~ a)

instance Foldable Vec where
  foldMap = foldMapDefault
instance Functor Vec where
  fmap = fmapDefault
instance Traversable Vec where
  traverse _ Vec = pure Vec 

instance (Traversable n) => Foldable ((:~) n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor ((:~) n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable ((:~) n) where
  traverse f (x :~ y) = (:~) <$> traverse f x <*> f y


-- | An coordinate 'Axis' , labeled by an integer. 
-- Axis also carries v, the container type for its corresponding
-- vector. Therefore, An axis of one type can access only vectors
-- of a fixed dimension, but of arbitrary type.
data Axis v = Axis Int deriving (Eq,Ord,Show,Read)

-- | An object that allows component-wise access.
class (Traversable v) => Vector v where
  -- | Get a component within f, a context which allows 'Failure'.
  getComponent :: (Failure StringException f) => Axis v -> v a -> f a
  -- | Get a component. This computation may result in a runtime error,
  -- though, as long as the 'Axis' is generated from library functions
  -- such as 'compose', there will be no error.
  component :: Axis v -> v a -> a
  component axis vec = unsafePerformFailure $ getComponent axis vec
  -- | The dimension of the vector.
  dimension :: v a -> Int
  -- | Create a 'Vector' from a function that maps 
  -- axis to component.
  compose :: (Axis v -> a) -> v a
  

instance Vector Vec where
    getComponent axis Vec 
        = failureString $ "axis out of bound: " ++ show axis
    dimension _ = 0
    compose _ = Vec 

instance (Vector v) => Vector ((:~) v) where
    getComponent (Axis i) vx@(v :~ x) 
        | i==dimension vx - 1 = return x
        | True                = getComponent (Axis i) v
    dimension (v :~ _) = 1 + dimension v
    compose f = let
        xs = compose (\(Axis i)->f (Axis i)) in xs :~ f (Axis (dimension xs))

-- | 'VectorAdditive' is a 'Vector' whose components belongs to 'Additive.C', 
-- thus providing zero vectors as well as addition between vectors.

class (Vector v, Additive.C a) => VectorAdditive v a where
  -- | A vector whose components are all zero.
  zeroVector :: v a 

instance (Additive.C a) => VectorAdditive Vec a where
  zeroVector = Vec 

instance (Additive.C a, VectorAdditive v a) => VectorAdditive ((:~) v) a where
  zeroVector = zeroVector :~ Additive.zero



-- | 'VectorRing' is a 'Vector' whose components belongs to 'Ring.C', 
-- thus providing unit vectors.
class  (Vector v, Ring.C a, VectorAdditive v a) => VectorRing v a where
  -- | A vector where 'Axis'th component is unity but others are zero.
  getUnitVector :: (Failure StringException f) => Axis v -> f (v a)
  
  unitVector :: Axis v -> v a
  unitVector = unsafePerformFailure . getUnitVector
    
instance (Ring.C a) => VectorRing Vec a where
  getUnitVector axis
      = failureString $ "axis out of bound: " ++ show axis

instance (Ring.C a, VectorRing v a, VectorAdditive v a) 
    => VectorRing ((:~) v) a where
  getUnitVector axis@(Axis i) = ret
    where
      z = zeroVector
      d = dimension z
      ret
        | i < 0 || i >= d   = failureString $ "axis out of bound: " ++ show axis
        | i == d-1          = return $ zeroVector :~ Ring.one
        | 0 <= i && i < d-1 = liftM (:~ Additive.zero) $ getUnitVector (Axis i)
        | True              = return z 
        -- this last guard never matches, but needed to infer the type of z.

-- | Type synonyms
type Vec0 = Vec
type Vec1 = (:~) Vec0
type Vec2 = (:~) Vec1
type Vec3 = (:~) Vec2
type Vec4 = (:~) Vec3
