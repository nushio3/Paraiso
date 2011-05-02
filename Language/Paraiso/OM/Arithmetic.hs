{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.OM.Arithmetic 
    (
     Arity(..), arityI, arityO,
     Operator(..)
    ) where

import NumericPrelude hiding (Ordering(..), Eq(..), Ord(..))
import qualified NumericPrelude as P

class Arity a where
  arity :: a -> (Int, Int)

arityI, arityO :: (Arity a) => a -> Int
arityI = fst.arity
arityO = snd.arity
  
data Operator = 
  Add |
  Sub |
  Neg |
  Mul | 
  Div |
  Inv |
  EQ |
  NE |
  LT |
  LE |
  GT |
  GE |
  -- | x^y where y is an integer
  Ipow |
  -- | x^y where y is real number
  Pow |
  Madd |
  Msub |
  Nmadd |
  Nmsub |
  Sincos 
  deriving (P.Eq, P.Ord, P.Show, P.Read)

instance Arity Operator where
  arity a = case a of
    Add -> (2,1)
    Sub -> (2,1)
    Neg -> (1,1)
    Mul -> (2,1)
    Div -> (2,1)
    Inv -> (1,1)
    EQ -> (2,1)
    NE -> (2,1)
    LT -> (2,1)
    LE -> (2,1)
    GT -> (2,1)
    GE -> (2,1)
    Ipow -> (2,1)
    Pow -> (2,1)
    Madd -> (3,1)
    Msub -> (3,1)
    Nmadd -> (3,1)
    Nmsub -> (3,1)
    Sincos -> (1,2)

