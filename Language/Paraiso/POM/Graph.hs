{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.OM.Graph() where

import Language.Paraiso.Interval
import Language.Paraiso.Tensor
import qualified Data.Graph.Inductive as G


class OperandC opd where                
    typeStr :: opd -> String


data (Vector v, Ord s) => Operand v s c = Operand (v (Interval s))
instance  (Vector v, Ord s) => OperandC (Operand v s c) where
  typeStr (Operand a) = "toge"
                

newtype StaticID = StaticID String deriving (Eq, Ord, Show, Read)


class Arity a where
  arity :: a -> (Int, Int)

arityI, arityO :: (Arity a) => a -> Int
arityI = fst.arity
arityO = snd.arity
  

data Inst v = 
  Load StaticID |
  Store StaticID |
  Reduce ReduceOperation |
  Broadcast |
  Shift (v Int) |
  Arith Arithmetic

instance Arity (Inst v) where
  arity a = case a of
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
    Broadcast -> (1,1)
    Shift _   -> (1,1)
    Arith a   -> arity a
                   
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


data ReduceOperation = ReduceMax | ReduceMin | ReduceSum 

instance Arity Arithmetic where
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

