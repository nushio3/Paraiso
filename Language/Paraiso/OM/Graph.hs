{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing Orthotope Machine data flow draph.
module Language.Paraiso.OM.Graph
    (
     Setup(..), Kernel(..), Graph,
     Name(..), Named(..), NamedValue(..), Annotation(..),
     Node(..), 
     Inst(..)
    )where

import qualified Algebra.Ring as Ring
import Data.Dynamic
import qualified Data.Graph.Inductive as G
import Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Reduce as R
import Language.Paraiso.OM.DynValue
import Language.Paraiso.Tensor
import NumericPrelude


-- | An OM Setup, a set of information needed before you start building a 'Kernel'.
-- It's basically a list of static orthotopes 
-- (its identifier, Realm and Type carried in the form of 'NamedValue')
data  (Vector vector, Ring.C gauge) => Setup vector gauge  = 
  Setup {
    staticValues :: [NamedValue]
  } deriving (Eq, Show)

-- | A 'Kernel' for OM does a bunch of calculations on OM.
data (Vector vector, Ring.C gauge) => Kernel vector gauge a = 
  Kernel {
    kernelName :: Name,
    dataflow :: Graph vector gauge a
  }         
    deriving (Show)
instance (Vector v, Ring.C g) => Named (Kernel v g a) where
  name = kernelName

-- | name identifier.
newtype Name = Name String deriving (Eq, Show)
class Named a where
  name :: a -> Name
  nameStr :: a -> String
  nameStr = (\(Name str) -> str) . name
instance Named Name where
  name = id

-- | a 'DynValue' with a specific name.
data NamedValue = NamedValue Name DynValue deriving (Eq, Show)
instance Named NamedValue where
  name (NamedValue n _) = n


-- | The dataflow graph for Orthotope Machine. a is an additional annotation.

type Graph vector gauge a = G.Gr (Node vector gauge a) ()

-- | The node for the dataflow 'Graph' of the Orthotope machine.
-- The dataflow graph is a 2-part graph consisting of 'NValue' and 'NInst' nodes.
data (Vector vector, Ring.C gauge) => Node vector gauge a = 
  -- | A value node. An 'NValue' node only connects to 'NInst' nodes.
  -- An 'NValue' node has one and only one input edge, and has arbitrary number of output edges.
  NValue DynValue a |
  -- | An instruction node. An 'NInst' node only connects to 'NValue' nodes.
  -- The number of input and output edges an 'NValue' node has is specified by its 'Arity'.
  NInst (Inst vector gauge) a
        deriving (Show)


data Inst vector gauge = 
  Imm Dynamic |
  Load Name |
  Store Name |
  Reduce R.Operator |
  Broadcast |
  Shift (vector gauge) |
  LoadIndex (Axis vector) |
  Arith A.Operator 
        deriving (Show)

instance Arity (Inst vector gauge) where
  arity a = case a of
    Imm _     -> (0,1)
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
    Broadcast -> (1,1)
    Shift _   -> (1,1)
    LoadIndex _ -> (0,1)
    Arith op  -> arity op


-- | you can insert 'Annotation's to control the code generation processes.
data Annotation = Comment String | Balloon
                deriving (Eq, Ord, Read, Show)

