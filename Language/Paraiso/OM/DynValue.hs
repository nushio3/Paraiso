{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the OM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.OM.DynValue
  (
   DynValue(..), toDyn
  ) where

import Data.Typeable
import qualified Language.Paraiso.OM.Realm as R

-- | dynamic value type, with its realm and content type informed as values
data DynValue = DynValue {realm :: R.Realm, typeRep :: TypeRep} deriving (Eq, Show)

toDyn :: (R.TRealm r, Typeable c) => r -> c -> DynValue
toDyn r0 c0 = DynValue (R.tRealm r0) (typeOf c0)

instance R.Realmable DynValue where
  realm = realm