{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the OM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.OM.DynValue
  (
   DynValue(..), mkDyn, toDyn
  ) where

import Data.Typeable
import qualified Language.Paraiso.OM.Value as Val
import qualified Language.Paraiso.OM.Realm as R

-- | dynamic value type, with its realm and content type informed as values
data DynValue = DynValue {realm :: R.Realm, typeRep :: TypeRep} deriving (Eq, Show)

-- | Make 'DynValue' value-level type, from the pair of Type-level type.
mkDyn :: (R.TRealm r, Typeable c) => r -> c -> DynValue
mkDyn r0 c0 = DynValue (R.tRealm r0) (typeOf c0)

-- | Convert 'Val.Value' to 'DynValue'
toDyn :: (R.TRealm r, Typeable c) => Val.Value r c -> DynValue
toDyn x = mkDyn (Val.realm x) (Val.content x)

instance R.Realmable DynValue where
  realm = realm