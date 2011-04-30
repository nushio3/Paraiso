{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | Expression Tree for type a
module Language.Paraiso.POM.Expr (Expr(..)) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Language.Paraiso.POM.Arithmetic as A
import NumericPrelude

data Expr a = Term a | Expr A.Operator [Expr a]

instance Additive.C a => Additive.C (Expr a) where
  zero     = Term zero
  x + y    = Expr A.Add [x, y]
  x - y    = Expr A.Sub [x, y]
  negate x = Expr A.Neg [x]
  
instance Ring.C a => Ring.C (Expr a) where
  one         = Term one
  x * y       = Expr A.Mul [x, y]
  fromInteger = Term . fromInteger
  x ^ y       = Expr Ipow [x, fromInteger y]
  
