{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, StandaloneDeriving, TypeFamilies,
  TypeOperators, UndecidableInstances #-} 
{-OPTIONS -Wall #-}

import Control.Applicative
import Control.Monad.Failure
import Data.Foldable
import Data.Traversable
import System.IO.Unsafe
import Prelude hiding(mapM)


import Control.Monad
import Control.Exception
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


data Zero = Zero deriving (Eq,Ord,Show,Read)
data Succ n = Succ n deriving (Eq,Ord,Show,Read)
data Ax n = Ax n Int deriving (Eq,Ord,Show,Read)

class Vector v  where
  type Dimension v
  type Axis v 
  getComponent :: (Failure StringException f) => Axis v -> v a -> f a
  getComponent':: (Failure StringException f) => Int -> v a -> f a

  unsafeComponent :: Axis v -> v a -> a
  unsafeComponent axis vec = unsafePerformFailure $ getComponent axis vec

  dimension :: v a -> Int

  compose :: (Axis v -> a) -> v a
  compose = undefined

instance Vector Vec where
  type Dimension Vec = Succ Zero
  type Axis Vec = Ax (Dimension Vec)
  getComponent axis@(Ax _ i) v = getComponent' i v

  getComponent' i (Vec x)
    | i==0 = return x
    | True = failureString $ "axis out of bound: " ++ show i

  dimension _ = 1


instance (Vector v) => Vector ((:~) v) where
  type Dimension ((:~) v) = Succ (Dimension v)
  type Axis ((:~) v) = Ax (Dimension ((:~) v))
  getComponent axis@(Ax _ i) v = getComponent' i v

  getComponent' i  vx@(v :~ x)  
    | i==dimension vx - 1 = return x
    | True                = getComponent' i v
  dimension (v :~ _) = 1 + dimension v




v1 :: Vec Int
v1 = Vec 0


v4 :: (:~) ((:~) Vec) :~ Int
v4 = Vec 1 :~ 3 :~ 4 :~ 1


v4a = compose (\i -> unsafeComponent i v4)

main :: IO ()
main = do
  print $ v1
  getComponent (Ax (Succ Zero) 0) v1 >>= print
  Control.Monad.forM_  [0..3] (\i-> getComponent (Ax (Succ (Succ (Succ (Succ Zero)))) i) v4 >>= print)
  return () 
