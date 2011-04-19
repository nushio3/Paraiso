{-# LANGUAGE FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, StandaloneDeriving, TypeFamilies,
  TypeOperators #-} 
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


-- | An coordinate 'Axis' , labeled by an integer. 
class Axis ax where
  data Vector :: * -> *
  getComponent :: ax -> Vector a -> a

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
  return ()
