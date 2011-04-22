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
  type Vector ax :: * -> *
  getComponent :: ax -> Vector ax a -> a
  getIndex :: ax -> Int
  dimension :: ax -> Int
  intToAxis :: Int -> ax
  compose :: (ax -> a) -> Vector ax a

data Zax = Zax Int deriving (Eq, Ord, Read, Show)
data Sax a = Sax a deriving (Eq, Ord, Read, Show)

instance Axis Zax where
  type Vector Zax = Vec
  getComponent ax (Vec x) 
    | getIndex ax == 0 = x
    | True             = error "Axis out of bound"
  getIndex (Zax i) = i
  dimension _ = 1
  intToAxis i = Zax i
  compose f = undefined

instance (Axis ax) => Axis (Sax ax) where
  type Vector (Sax ax) = (:~) (Vector ax)
  getComponent ax@(Sax ax') (v :~ x) 
    | getIndex ax == dimension ax - 1 = x
    | True                            = getComponent ax' v
  getIndex (Sax ax) = getIndex ax
  dimension (Sax ax) = 1 + dimension ax
  intToAxis i = Sax (intToAxis i)
  compose f = undefined


v1 :: Vec Int
v1 = Vec 0

v2 :: Vec :~ Int
v2 =  Vec 4 :~ 2

v4 :: (:~) ((:~) Vec) :~ Int
v4 = Vec 1 :~ 3 :~ 4 :~ 1


--v41 = compose (\i -> getComponent i v4)

{-
*Main> :t v4
v4 :: (:~) ((:~) Vec) :~ Int
*Main> :t (\i -> getComponent i v4)
(\i -> getComponent i v4)
  :: (Vector ax ~ (:~) ((:~) ((:~) Vec)), Axis ax) => ax -> Int
*Main> :t compose
compose :: Axis ax => (ax -> a) -> Vector ax a
*Main>  compose (\i -> getComponent i v4)

<interactive>:1:31:
    Couldn't match type `Vector ax0' with `(:~) ((:~) ((:~) Vec))'
    Expected type: Vector ax0 Int
      Actual type: (:~) ((:~) Vec) :~ Int
    In the second argument of `getComponent', namely `v4'
    In the expression: getComponent i v4
    In the first argument of `compose', namely
      `(\ i -> getComponent i v4)'


-}

main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  print [getComponent (intToAxis i::(Sax (Sax (Sax (Zax))))) v4|i<-[0..3]]
  return ()
