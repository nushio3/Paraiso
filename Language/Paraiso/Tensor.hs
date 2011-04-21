{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-} 
{-# OPTIONS -Wall #-}

module Language.Paraiso.Tensor
    (
     (:~)(..), Vec(..), Axis(..), Vector(..), VectorNum(..),
     Vec1, Vec2, Vec3, Vec4
    ) where

import Control.Applicative
import Control.Monad.Failure
import Data.Foldable
import Data.Traversable
import System.IO.Unsafe
import Prelude hiding(mapM)


import Control.Monad
unsafePerformFailure :: IO a -> a
unsafePerformFailure = unsafePerformIO


data Vec a = Vec 
infixl 3 :~
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
-- | Axis also carries v, the container type for its corresponding
-- | vector. Therefore, An axis of one type can access only vectors
-- | of a fixed dimension.
data Axis v = Axis Int deriving (Eq,Ord,Show,Read)

class Vector v where
  getComponent :: (Failure StringException f) => Axis v -> v a -> f a
  component :: Axis v -> v a -> a
  component axis vec = unsafePerformFailure $ getComponent axis vec
  dimension :: v a -> Int
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

-- | 'VectorNum' is a 'Vector' whose components are of instance 'Num'.
class  (Vector v, Num a) => VectorNum v a where
  -- | A vector whose components are all zero.
  zeroVector :: v a 

  -- | A vector where 'Axis'th component is unity but others are zero.
  getUnitVector :: (Failure StringException f) => Axis v -> f (v a)
  
  unitVector :: Axis v -> v a
  unitVector = unsafePerformFailure . getUnitVector
    
instance (Num a) => VectorNum Vec a where
  zeroVector = Vec 
  getUnitVector axis
      = failureString $ "axis out of bound: " ++ show axis

instance (Num a, VectorNum v a) => VectorNum ((:~) v) a where
  zeroVector = zeroVector :~ 0  

  getUnitVector axis@(Axis i) = ret
    where
      z = zeroVector
      d = dimension z
      ret
        | i < 0 || i >= d   = failureString $ "axis out of bound: " ++ show axis
        | i == d-1          = return $ zeroVector :~ 1
        | 0 <= i && i < d-1 = liftM (:~0) $ getUnitVector (Axis i)
        | True              = return z

type Vec0 = Vec
type Vec1 = (:~) Vec0
type Vec2 = (:~) Vec1
type Vec3 = (:~) Vec2
type Vec4 = (:~) Vec3
