{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that describes the dependency of the nodes
-- and labels certain group of Manifest nodes
-- that can safely be accessed simultaneously

module Language.Paraiso.Annotation.Dependency (
  Direct(..),
  Calc(..),
  Indirect(..),
  KernelWriteGroup(..), 
  OMWriteGroup(..)
  ) where


import           Data.Dynamic
import qualified Data.Graph.Inductive as FGL
import qualified Data.Set as Set
import           Language.Paraiso.Prelude

-- | The list of Manifest or Existing nodes that this node directly depends on.
-- Y directly depends on X if you need to read X in subroutine you calculate Y
newtype Direct
  = Direct [FGL.Node]
  deriving (Eq, Show, Typeable)
           
-- | The list of Manifest or Existing nodes that this node indirectly depends on.
-- Y indirectly depends on X if you need to calculate X before you calculace Y
newtype Indirect
  = Indirect [FGL.Node]
  deriving (Eq, Show, Typeable)

-- | The list of All nodes that this node directly depends on.
-- Y directly depends on X if you need to calculate X in subroutine you calculate Y
newtype Calc
  = Calc (Set.Set FGL.Node)
  deriving (Eq, Show, Typeable)



-- | Write grouping, continuously numbered from [0 ..] .
-- The numbering starts from 0 for each kerenel in a Orthotope Machine.
data KernelWriteGroup
  = KernelWriteGroup {getKernelGroupID :: Int}
  deriving (Eq, Show, Typeable)

-- | Write grouping, continuously numbered from [0 ..] .
-- The numbering is unique in one Orthotope Machine.
data OMWriteGroup
  = OMWriteGroup {getOMGroupID :: Int}
  deriving (Eq, Show, Typeable)

