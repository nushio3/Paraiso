{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that selects whether the data should be 
-- stored globally on memory or to be calculated.

module Language.Paraiso.Annotation.Allocation (
  Allocation(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Prelude
0
data Allocation 
  = Manifest 
  | Delayed 
  deriving (Eq, Show, Typeable)
