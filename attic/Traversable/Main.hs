{-# OPTIONS -Wall #-}

import Control.Applicative
import Data.Foldable
import Data.Traversable

newtype Triplet a = Triplet (a,a,a)

instance Foldable Triplet where
  foldMap = foldMapDefault

instance Functor Triplet where
  fmap = fmapDefault

instance Traversable Triplet where
  traverse f (Triplet (a1,a2,a3)) = fmap Triplet $
    (,,) <$> f a1 <*> f a2 <*> f a3


main :: IO ()
main = do
  _ <- Data.Traversable.mapM putStrLn $ t3
  _ <- Data.Traversable.mapM (Data.Traversable.mapM putStrLn) $ t9
  return ()
    where
      t3 = Triplet ("hoge","huga","piyo")
      a  = fmap (++"ra") t3
      b  = fmap (++"ga") t3
      c  = fmap (++"zaga") t3
      t9 = Triplet (a,b,c)
      
