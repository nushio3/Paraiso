{-# OPTIONS -Wall #-}

import Control.Applicative
import Data.Foldable
import Data.Traversable

newtype Triplet a = Triplet (a,a,a)

instance Foldable Triplet where
  foldr f b (Triplet (a1,a2,a3)) = Prelude.foldr f b [a1,a2,a3]

instance Functor Triplet where
  fmap f (Triplet (a1,a2,a3)) = Triplet (f a1, f a2, f a3)

instance Traversable Triplet where
  traverse f (Triplet (a1,a2,a3)) = fmap Triplet $
    (,,) <$> f a1 <*> f a2 <*> f a3

main :: IO ()
main = do
  _ <- Data.Traversable.mapM putStrLn $ Triplet ("hoge","huga","piyo")
  return ()

