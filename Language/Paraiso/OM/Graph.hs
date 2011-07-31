{-# 
LANGUAGE ExistentialQuantification,  KindSignatures, 
         NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | the components for constructing Orthotope Machine data flow draph.
--  Most components take three arguments: 
--
-- [@vector :: * -> *@] The array dimension. It is a 'Vector' that
-- defines the dimension of the Orthotope on which the OM operates.
--
-- [@gauge :: *@] The array index. The combination @vector gauge@
-- needs to be an instance of 'Algebra.Additive.C' if you want to
-- perform @Shift@ operation.
-- 
-- [@anot :: *@] The annotations put on each node. If you want to use
-- Annotation, @anot@ needs to be an instance of 'Data.Monoid'.


module Language.Paraiso.OM.Graph
    (
     Setup(..), Kernel(..), Graph, nmap, getA,
     Node(..), Edge(..),
     Inst(..),
    )where

import Data.Dynamic
import qualified Data.Graph.Inductive as FGL
import Language.Paraiso.Name
import Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Reduce as R
import Language.Paraiso.OM.DynValue
import Language.Paraiso.Tensor
import NumericPrelude


-- | An OM Setup, a set of information needed before you start building a 'Kernel'.
data Setup (vector :: * -> *) gauge anot = 
  Setup {
    -- | The list of static orthotopes 
    --  (its identifier, Realm and Type carried in the form of 'NamedValue')
    staticValues :: [Named DynValue], 
    -- | The machine-global annotations
    globalAnnotation :: anot          
  } deriving (Eq, Show)

-- | A 'Kernel' for OM perfor a block of calculations on OM.
data  Kernel vector gauge anot = 
  Kernel {
    kernelName :: Name,
    dataflow :: Graph vector gauge anot
  }         
    deriving (Show)

instance Nameable (Kernel v g a) where
  name = kernelName


-- | The dataflow graph for Orthotope Machine. anot is an additional annotation.
type Graph vector gauge anot = FGL.Gr (Node vector gauge anot) Edge

-- | Map the 'Graph' annotation from one type to another. Unfortunately we cannot make one data
-- both the instances of 'FGL.Graph' and 'Functor', so 'nmap' is a standalone function.
nmap :: (a -> b) -> Graph v g a ->  Graph v g b
nmap f = let
    nmap' f0 (NValue x a0) = (NValue x $ f0 a0) 
    nmap' f0 (NInst  x a0) = (NInst  x $ f0 a0) 
  in FGL.nmap (nmap' f)


-- | The 'Node' for the dataflow 'Graph' of the Orthotope machine.
-- The dataflow graph is a 2-part graph consisting of 'NValue' and 'NInst' nodes.
data Node vector gauge anot = 
  -- | A value node. An 'NValue' node only connects to 'NInst' nodes.
  -- An 'NValue' node has one and only one input edge, and has arbitrary number of output edges.
  NValue DynValue anot |
  -- | An instruction node. An 'NInst' node only connects to 'NValue' nodes.
  -- The number of input and output edges an 'NValue' node has is specified by its 'Arity'.
  NInst (Inst vector gauge) anot
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
  
instance Functor (Node v g) where
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


