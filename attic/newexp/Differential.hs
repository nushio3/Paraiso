{-# LANGUAGE ScopedTypeVariables #-}
module Differential where

import Control.Applicative
import Data.Dynamic
import Data.Ratio

import Expr
import Transformation
import Tensor

-- A symbolic partial differentiation.
partial :: forall a. (Typeable a) => Expr Axis -> Expr (Pt -> a) -> Expr (Pt -> a)
partial i f = (Reserved Partial :: Expr (Axis -> (Pt->a)->(Pt->a))) :$ i :$ f

ә :: forall a. (Typeable a) => Expr Axis -> Expr (Pt -> a) -> Expr (Pt -> a)
ә = partial




-- A 4-th order implementation of partial differentiation.
partial4 :: (Typeable a, Fractional a) => Axis -> Expr (Pt->a) -> (Expr Pt-> Expr a)
partial4 i f p = (fromRational $ 9%8)*((f :$ (p + 0.5 * e(i))) - (f :$ (p - 0.5 * e(i)))) 
                 - (fromRational $ 1%24)*((f :$ (p + 1.5 * e(i))) - (f :$ (p - 1.5 * e(i)))) 
  where
    e :: Axis -> Expr Pt
    e i = Static ("\\mathbf{e}_"++show i) (\j -> if i==j then 1 else 0)



usePartial4 :: (Typeable a, Fractional a) => Expr a -> Expr a 
usePartial4 x = case x of
  (Reserved Partial :$ i :$ f :$ r) -> case partial4 <$> (runStatic i >>= cast) <*> cast f <*> cast r of
    Just y -> y
    _      -> x
  _ -> x
