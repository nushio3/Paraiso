{-# OPTIONS -Wall #-}

-- | The 'DynValue' is stored in the OM dataflow graph type.
-- 'DynValue' carries the type and homogeneity information in value.
-- Therefore, 'DynValue' with various types can be stored in single container type such as graph.

module Language.Paraiso.OM.DynValue
  (
   DynValue(..), mkDyn, toDyn, f2d,  ToDynable 
  ) where

import Data.Typeable
import qualified Language.Paraiso.OM.Value as Val
import qualified Language.Paraiso.OM.Realm as R

-- | dynamic value type, with its realm and content type informed as values
data DynValue = DynValue {realm :: R.Realm, typeRep :: TypeRep} deriving (Eq, Show)

-- | Make 'DynValue' value-level type, from the pair of Type-level type.
mkDyn :: (R.TRealm r, Typeable c) => r -> c -> DynValue
mkDyn r0 c0 = DynValue (R.tRealm r0) (typeOf c0)


-- | Something that can be converted to 'DynValue'
class ToDynable a where
  toDyn :: a -> DynValue

-- | Convert 'Val.Value' to 'DynValue'
instance (R.TRealm r, Typeable c) => ToDynable (Val.Value r c) where
  toDyn x = mkDyn (Val.realm x) (Val.content x)

-- | Convert 'Val.StaticValue' to 'DynValue'
instance (R.TRealm r, Typeable c) => ToDynable (Val.StaticValue r c) where
  toDyn (Val.StaticValue r c) = mkDyn r c

-- | map 'toDyn' over functors.
--   an idiom used in collecting OM static variables.
f2d :: (Functor f, ToDynable x) => f x -> f DynValue
f2d = fmap toDyn



instance R.Realmable DynValue where
  realm = realm