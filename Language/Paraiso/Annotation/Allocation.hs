{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that selects whether the data should be 
-- stored globally on memory or to be calculated.

module Language.Paraiso.Annotation.Allocation (
  Allocation(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Prelude

data Allocation 
  = Existing -- ^ This entity is already allocated as a static variable.
  | Manifest -- ^ Allocate additional memory for this entity. 
  | Delayed  -- ^ Do not allocate, re-compute it whenever if needed.
  deriving (Eq, Show, Typeable)
