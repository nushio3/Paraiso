{-# OPTIONS -Wall #-}
module Language.Paraiso.POM.Arithmetic 
    (
     Arity(..), arityI, arityO,
     Operator(..)
    ) where


class Arity a where
  arity :: a -> (Int, Int)

arityI, arityO :: (Arity a) => a -> Int
arityI = fst.arity
arityO = snd.arity
  
data Operator = 
  Imm |
  Add |
  Sub |
  Neg |
  Mul | 
  Div |
  Inv |
  Madd |
  Msub |
  Nmadd |
  Nmsub |
  Sincos 
  deriving (Eq, Ord, Show, Read)

instance Arity Operator where
  arity a = case a of
    Imm -> (0,1)
    Add -> (2,1)
    Sub -> (2,1)
    Neg -> (1,1)
    Mul -> (2,1)
    Div -> (2,1)
    Inv -> (1,1)
    Madd -> (3,1)
    Msub -> (3,1)
    Nmadd -> (3,1)
    Nmsub -> (3,1)
    Sincos -> (1,2)

