{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS -Wall #-}

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Prelude hiding(mapM)

data Z a = Z a 
infixl 3 :~
data n :~ a = (n a) :~ a

deriving instance (Show a) => Show (Z a)
deriving instance (Show a, Show (n a)) => Show (n :~ a)

instance Foldable Z where
  foldMap = foldMapDefault
instance Functor Z where
  fmap = fmapDefault
instance Traversable Z where
  traverse f (Z x) = Z <$> f x

instance (Traversable n) => Foldable ((:~) n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor ((:~) n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable ((:~) n) where
  traverse f (x :~ y) = (:~) <$> traverse f x <*> f y

v1 :: Z Int
v1 = Z 0

v2 :: Z :~ Int
v2 =  Z 4 :~ 2

v4 :: (:~) ((:~) Z) :~ Int
v4 = Z 1 :~ 3 :~ 4 :~ 1


main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  _ <- mapM print v4
  return ()
