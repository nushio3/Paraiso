{-# OPTIONS -Wall #-}

import qualified Data.Graph.Inductive as G


data NodeL = NodeL {inst::Inst, anot::Annotation}
data EdgeL = EdgeL

type Gr = G.Gr NodeL EdgeL

data Operator = 
    Add |
    Mul | 
    Sub |
    Div |
    Inv |
    Neg |
    Madd |
    Msub |
    Nmadd |
    Nmsub 



arity :: Operator -> (Int, Int) 



arityI, arityO :: Operator -> Int
arityI = fst.arity
arityO = snd.arity
  