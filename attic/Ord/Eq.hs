{-# LANGUAGE   DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances , FunctionalDependencies ,  MultiParamTypeClasses , GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

import Control.Applicative

class Hikaku a b | a->b where
  chisai :: a -> a -> b
  
class Shakkuri a where
  shakkuri :: a -> String

instance Shakkuri Int where
  shakkuri x = concat $ replicate x "hick!"

newtype Expr a = Expr a
            deriving (Eq, Ord, Show, Functor, Shakkuri)


instance Applicative Expr where
  pure = Expr
  (<*>) (Expr f) (Expr x) = Expr (f x)

instance Ord a => Hikaku a Bool where
  chisai x y = x < y
  
main = do  
  print $ 2 `chisai` 3
  print ((<) <$> Expr 2 <*> Expr 3 )
  putStrLn $ shakkuri $ Expr (3::Int)