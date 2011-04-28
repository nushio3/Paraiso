{-# LANGUAGE ExistentialQuantification,  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | all the components for constructing POM data flow draph.
module Language.Paraiso.POM.Graph
    (
     Homogeneous, Inhomogeneous, Homogeneity,
     StaticID(..), Annotation(..),
     Operand(..), POMNode(..)
    )where

import qualified Algebra.Ring as Ring
import Data.Typeable
import Language.Paraiso.POM.Arithmetic as A
import Language.Paraiso.POM.Reduce as R
import Language.Paraiso.Tensor
import NumericPrelude

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

data (Homogeneity hom, Typeable content) => 
  Operand hom content = Operand hom Int

data Inst vector gauge = 
  Load StaticID |
  Store StaticID |
  Reduce R.Operator |
  Shift (vector gauge) |
  Arith A.Operator

instance Arity (Inst vector gauge) where
  arity a = case a of
    Load _    -> (0,1)
    Store _   -> (1,0)
    Reduce _  -> (1,1)
    Shift _   -> (1,1)
    Arith op  -> arity op


data Homogeneous 
data Inhomogeneous
class Homogeneity a where
  homogeneity :: a -> Bool
  


instance Homogeneity Homogeneous where
  homogeneity _ = True
instance Homogeneity Inhomogeneous where
  homogeneity _ = False
