{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM(POM(..)) where

import qualified Algebra.Ring as Ring
import qualified Data.Graph.Inductive as G
import Data.Typeable
import Language.Paraiso.Interval
import Language.Paraiso.Tensor
import NumericPrelude

data POM = POM



data Homogeneous 
data Inhomogeneous
class Homogeneity a where
  homogeneity :: a -> Bool
  
instance Homogeneity Homogeneous where
  homogeneity _ = True
instance Homogeneity Inhomogeneous where
  homogeneity _ = False

data (Vector vector, Ring.C size, Homogeneity hom, Typeable content) => 
  Operand vector size hom content = Operand Int

data (Vector vector, Ring.C size) => OMNode vector size = 
  NOperand {
    opType :: TypeRep,
    extent :: vector (Interval size),
    homo   :: Bool
  } |
  NOperator {
    inst :: Inst vector size
  }



newtype StaticID = StaticID String deriving (Eq, Ord, Show, Read)


class Arity a where
  arity :: a -> (Int, Int)

arityI, arityO :: (Arity a) => a -> Int
arityI = fst.arity
arityO = snd.arity
  

data Inst vector size = 
  Load StaticID |
  Store StaticID |
  Reduce ReduceOperation |
  Shift (vector size) |
  Arith Arithmetic

instance Arity (Inst vector size) where
  arity a = case a of
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
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

