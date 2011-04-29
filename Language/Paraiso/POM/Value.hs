{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the POM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.POM.Value
  (
   Homogeneous(..), Inhomogeneous(..), Homogeneity(..),
   Value(..), 
  ) where

import Data.Dynamic
import Data.Typeable
import qualified Data.Graph.Inductive as G
import NumericPrelude


data Homogeneous 
data Inhomogeneous
class Homogeneity a where
  homogeneity :: a -> Bool
  

instance Homogeneity Homogeneous where
  homogeneity _ = True
instance Homogeneity Inhomogeneous where
  homogeneity _ = False
  
data (Homogeneity hom, Typeable content) => 
  Value hom content = FromNode hom G.Node | Imm hom TypeRep Dynamic
  