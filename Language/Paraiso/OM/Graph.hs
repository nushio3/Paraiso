{-# OPTIONS -Wall #-}
module Language.Paraiso.OM.Graph() where


import qualified Data.Graph.Inductive as G


data NLabel v = NOperand {operandID::Int, anots::[Annotation]} |
               NOperator {inst::Inst v, anots::[Annotation]}
data ELabel v = ELabel

-- | Graph that represents calculations on v-dimensional orthotope
type Gr v = G.Gr (NLabel v) (ELabel v)

data Inst v = 
  Reduce |
  Broadcast |
  Shift (v Int) |
  Arith Arithmetic


data Annotation = Comment String | Balloon

data Arithmetic = 
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



arity :: Arithmetic -> (Int, Int) 

arity a = case a of
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

arityI, arityO :: Arithmetic -> Int
arityI = fst.arity
arityO = snd.arity
  
