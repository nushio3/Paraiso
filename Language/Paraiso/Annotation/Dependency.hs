{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that describes the dependency of the nodes
-- and labels certain group of Manifest nodes
-- that can safely be accessed simultaneously

module Language.Paraiso.Annotation.Dependency (
  KernelWriteGroup(..), 
  OMWriteGroup(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Prelude

-- | Write grouping, continuously numbered from [0 ..] .
-- The numbering starts from 0 for each kerenel in a Orthotope Machine.
data KernelWriteGroup
  = KernelWriteGroup Int
  deriving (Eq, Show, Typeable)

-- | Write grouping, continuously numbered from [0 ..] .
-- The numbering is unique in one Orthotope Machine.
data OMWriteGroup
  = OMWriteGroup Int
  deriving (Eq, Show, Typeable)

