{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing Orthotope Machine data flow draph.
module Language.Paraiso.OM.Graph
    (
     Setup(..), Kernel(..),
     Name(..), NamedValue(..), Annotation(..),
     Node(..), Graph
    )where

import qualified Algebra.Ring as Ring
import Data.Dynamic
import qualified Data.Graph.Inductive as G
import Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Reduce as R
import Language.Paraiso.OM.DynValue
import Language.Paraiso.Tensor
import NumericPrelude


-- | OM setup.
data  (Vector vector, Ring.C gauge) => Setup vector gauge  = 
  Setup {
    staticValues :: [NamedValue]
  } deriving (Eq, Show)

-- | A Kernel for OM.
data (Vector vector, Ring.C gauge) => Kernel vector gauge a = 
  Kernel {
    dataflow :: Graph vector gauge a
  }         
    deriving (Show)


-- | The dataflow graph for Orthotope Machine. a is an additional annotation.
type Graph vector gauge a = G.Gr (Node vector gauge a) ()

newtype Name = Name String deriving (Eq, Show)
data NamedValue = NamedValue Name DynValue deriving (Eq, Show)

data Annotation = Comment String | Balloon
                deriving (Eq, Ord, Read, Show)

data (Vector vector, Ring.C gauge) => Node vector gauge a = 
  NValue DynValue a |
  NInst (Inst vector gauge)
        deriving (Show)

data Inst vector gauge = 
  Imm TypeRep Dynamic |
  Load Name |
  Store Name |
  Reduce R.Operator |
  Shift (vector gauge) |
  Arith A.Operator 
        deriving (Show)

instance Arity (Inst vector gauge) where
  arity a = case a of
    Imm _ _   -> (0,1)
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
    Shift _   -> (1,1)
    Arith op  -> arity op



