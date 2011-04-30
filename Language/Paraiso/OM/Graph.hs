{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing POM data flow draph.
module Language.Paraiso.OM.Graph
    (
     StaticID, StaticValue(..), Annotation(..),
     POMNode(..), POMGraph
    )where

import qualified Algebra.Ring as Ring
import Data.Dynamic
import qualified Data.Graph.Inductive as G
import Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Reduce as R
import Language.Paraiso.OM.DynValue
import Language.Paraiso.Tensor
import NumericPrelude

-- | The dataflow graph for POM. a is an additional annotation.
type POMGraph vector gauge a = G.Gr (POMNode vector gauge a) ()

type StaticID = String
data StaticValue = StaticValue StaticID DynValue deriving (Eq, Show)

data Annotation = Comment String | Balloon


data (Vector vector, Ring.C gauge) => POMNode vector gauge a = 
  NValue DynValue a |
  NInst (Inst vector gauge)
 

data Inst vector gauge = 
  Imm TypeRep Dynamic |
  Load StaticID |
  Store StaticID |
  Reduce R.Operator |
  Shift (vector gauge) |
  Arith A.Operator

instance Arity (Inst vector gauge) where
  arity a = case a of
    Imm _ _   -> (0,1)
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
    Shift _   -> (1,1)
    Arith op  -> arity op



