{-# LANGUAGE StandaloneDeriving, FlexibleContexts #-}
{-# OPTIONS -Wall #-}

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Prelude hiding(mapM)

data Zero a = Zero a 
data Succ n a = Succ (n a) a

deriving instance (Show a) => Show (Zero a)
deriving instance (Show a, Show (n a)) => Show (Succ n a)

instance Foldable Zero where
  foldMap = foldMapDefault
instance Functor Zero where
  fmap = fmapDefault
instance Traversable Zero where
  traverse f (Zero x) = Zero <$> f x

instance (Traversable n) => Foldable (Succ n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor (Succ n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable (Succ n) where
  traverse f (Succ x y) = Succ <$> traverse f x <*> f y

v1 :: Zero Int
v1 = Zero 0

v2 :: Succ Zero Int
v2 = Succ (Zero 4) 2

v4 :: Succ (Succ (Succ Zero)) Int
v4 = Succ (Succ (Succ (Zero 1) 3) 4) 1


main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  _ <- mapM print v4
  return ()
