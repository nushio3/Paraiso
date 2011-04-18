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
data Axis = Axis Int deriving (Eq,Ord,Show,Read)

class Vector v where
  getComponent :: (Failure StringException f) => Axis -> v a -> f a
  unsafeComponent :: Axis -> v a -> a
  unsafeComponent axis vec = unsafePerformFailure $ getComponent axis vec
  dimension :: v a -> Int


instance Vector Vec where
    getComponent axis@(Axis i) (Vec x) 
        | i==0 = return x
        | True = failureString $ "axis out of bound: " ++ show axis
    dimension _ = 1

instance (Vector v) => Vector ((:~) v) where
    getComponent axis@(Axis i) vx@(v :~ x) 
        | i==dimension vx - 1 = return x
        | True                = getComponent axis v
    dimension (v :~ _) = 1 + dimension v


-- | 'VectorNum' is a 'Vector' whose components are of instance 'Num'.
class  (Vector v, Num a) => VectorNum v a where
  -- | A vector whose components are all zero.
  zeroVector :: v a 

  -- | A vector where 'Axis'th component is unity but others are zero.
  getUnitVector :: (Failure StringException f) => Axis -> f (v a)
  
  unsafeUnitVector :: Axis -> v a
  unsafeUnitVector = unsafePerformFailure . getUnitVector
    
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
        | 0 <= i && i < d-1 = liftM (:~0) $ getUnitVector axis
        | True              = return z

v1 :: Vec Int
v1 = Vec 0

v2 :: Vec :~ Int
v2 =  Vec 4 :~ 2

v4 :: (:~) ((:~) Vec) :~ Int
v4 = Vec 1 :~ 3 :~ 4 :~ 1



main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  _ <- Data.Traversable.mapM print v4
  Control.Monad.forM_  [0..3] (\i-> getComponent (Axis i) v4 >>= print)
  bases <- Control.Monad.forM [0..3] (\i-> getUnitVector (Axis i))
  print $ v4:zeroVector:bases
  return ()
