{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, StandaloneDeriving, TypeOperators #-} 
{-# OPTIONS -Wall #-}

import Control.Applicative
import Control.Monad.Failure
import Data.Foldable
import Data.Traversable
import System.IO.Unsafe
import Prelude hiding(mapM)


import Control.Monad
unsafePerformFailure :: IO a -> a
unsafePerformFailure = unsafePerformIO


data Vec a = Vec a 
infixl 3 :~
data n :~ a = (n a) :~ a

deriving instance (Show a) => Show (Vec a)
deriving instance (Show a, Show (n a)) => Show (n :~ a)

instance Foldable Vec where
  foldMap = foldMapDefault
instance Functor Vec where
  fmap = fmapDefault
instance Traversable Vec where
  traverse f (Vec x) = Vec <$> f x

instance (Traversable n) => Foldable ((:~) n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor ((:~) n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable ((:~) n) where
  traverse f (x :~ y) = (:~) <$> traverse f x <*> f y


-- | An coordinate 'Axis' , labeled by an integer. 
data Axis v = Axis Int deriving (Eq,Ord,Show,Read)

class Vector v where
  getComponent :: (Failure StringException f) => Axis v -> v a -> f a
  component :: Axis v -> v a -> a
  component axis vec = unsafePerformFailure $ getComponent axis vec
  dimension :: v a -> Int
  compose :: (Axis v -> a) -> v a
  

instance Vector Vec where
    getComponent axis@(Axis i) (Vec x) 
        | i==0 = return x
        | True = failureString $ "axis out of bound: " ++ show axis
    dimension _ = 1
    compose f = Vec (f (Axis 0))

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
  zeroVector = Vec 0
  getUnitVector axis@(Axis i) 
      | i == 0 = return $ Vec 1
      | True   = failureString $ "axis out of bound: " ++ show axis

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

type Vec1 = Vec
type Vec2 = (:~) Vec1
type Vec3 = (:~) Vec2
type Vec4 = (:~) Vec3

v1 :: Vec1 Int
v1 = Vec 0

v2 :: Vec2  Int
v2 =  Vec 4 :~ 2

v4 :: Vec4 Int
v4 = Vec 1 :~ 3 :~ 4 :~ 1

t4 :: Vec4 (Vec4 Int)
t4 = compose (\i -> compose (\j -> if i==j then 1 else 0))

main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  _ <- Data.Traversable.mapM print v4
  Control.Monad.forM_  [0..3] (\i-> getComponent (Axis i) v4 >>= print)
  bases <- Control.Monad.forM [0..3] (\i-> getUnitVector (Axis i))
  print $ v4:zeroVector:bases
  print $ compose (\i -> compose (\j -> component i v4 * component j v4 ))
  print $ t4
  return ()
