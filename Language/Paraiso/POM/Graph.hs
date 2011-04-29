{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing POM data flow draph.
module Language.Paraiso.POM.Graph
    (
     StaticID(..), Annotation(..),
     POMNode(..), POMGraph
    )where

import qualified Algebra.Ring as Ring
import Data.Dynamic
import qualified Data.Graph.Inductive as G
import Language.Paraiso.POM.Arithmetic as A
import Language.Paraiso.POM.Expr as E
import Language.Paraiso.POM.Reduce as R
import Language.Paraiso.Tensor
import NumericPrelude

type POMGraph vector gauge = G.Gr (POMNode vector gauge) ()

newtype StaticID = StaticID String deriving (Eq, Ord, Show, Read)
data Annotation = Comment String | Balloon


data (Vector vector, Ring.C gauge) => POMNode vector gauge = 
  NOperand {
    typeRep :: TypeRep,
    homo    :: Bool
  } |
  NInst {
    inst :: Inst vector gauge
  }

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



