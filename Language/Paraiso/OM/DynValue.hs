{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the OM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.OM.DynValue
  (
   DynValue(..)
  ) where

import Data.Typeable
import qualified Language.Paraiso.OM.Realm as R

-- | dynamic value type, with its realm and content type informed as values
data DynValue = DynValue {realm :: R.Realm, typeRep :: TypeRep} deriving (Eq, Show)

instance R.Realmable DynValue where
  realm = realm