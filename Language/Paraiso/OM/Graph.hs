{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude, 
  StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing Orthotope Machine data flow draph.
module Language.Paraiso.OM.Graph
    (
     Setup(..), Kernel(..), Graph, nmap, getA,
     Node(..), Edge(..),
     Inst(..),
     module Language.Paraiso.Name
    )where

import qualified Algebra.Ring as Ring
import Data.Dynamic
import qualified Data.Graph.Inductive as FGL
import Language.Paraiso.Name
import Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Reduce as R
import Language.Paraiso.OM.DynValue
import Language.Paraiso.Tensor
import NumericPrelude


-- | An OM Setup, a set of information needed before you start building a 'Kernel'.
-- It's basically a list of static orthotopes 
-- (its identifier, Realm and Type carried in the form of 'NamedValue')
data  (Vector vector, Ring.C gauge) => Setup vector gauge = 
  Setup {
    staticValues :: [Named DynValue]
  } deriving (Eq, Show)

-- | A 'Kernel' for OM does a bunch of calculations on OM.
data (Vector vector, Ring.C gauge) => Kernel vector gauge a = 
  Kernel {
    kernelName :: Name,
    dataflow :: Graph vector gauge a
  }         
    deriving (Show)

instance (Vector v, Ring.C g) => Nameable (Kernel v g a) where
  name = kernelName


-- | The dataflow graph for Orthotope Machine. a is an additional annotation.
type Graph vector gauge a = FGL.Gr (Node vector gauge a) Edge

-- | Map the 'Graph' annotation from one type to another. Unfortunately we cannot make one data
-- both the instances of 'FGL.Graph' and 'Functor', so 'nmap' is a standalone function.
nmap :: (Vector v, Ring.C g) => (a -> b) -> Graph v g a ->  Graph v g b
nmap f = let
    nmap' f0 (NValue x a0) = (NValue x $ f0 a0) 
    nmap' f0 (NInst  x a0) = (NInst  x $ f0 a0) 
  in FGL.nmap (nmap' f)


-- | The 'Node' for the dataflow 'Graph' of the Orthotope machine.
-- The dataflow graph is a 2-part graph consisting of 'NValue' and 'NInst' nodes.
data Node vector gauge a = 
  -- | A value node. An 'NValue' node only connects to 'NInst' nodes.
  -- An 'NValue' node has one and only one input edge, and has arbitrary number of output edges.
  NValue DynValue a |
  -- | An instruction node. An 'NInst' node only connects to 'NValue' nodes.
  -- The number of input and output edges an 'NValue' node has is specified by its 'Arity'.
  NInst (Inst vector gauge) a
        deriving (Show)

-- | The 'Edge' label for the dataflow 'Graph'. 
-- | It keeps track of the order of the arguments.
data Edge = 
    -- | an unordered edge.  
    EUnord | 
    -- | edges where the order matters.
    EOrd Int deriving (Eq, Ord, Show)

-- | get annotation of the node.
getA :: Node v g a -> a
getA nd = case nd of
  NValue _ x -> x
  NInst  _ x -> x
  


instance (Vector v, Ring.C g) => Functor (Node v g) where
  fmap f (NValue x y) =  (NValue x (f y))  
  fmap f (NInst  x y) =  (NInst  x (f y))  

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


