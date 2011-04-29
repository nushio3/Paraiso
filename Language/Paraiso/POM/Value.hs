{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the POM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.POM.Value
  (
   TGlobal(..), TLocal(..), TRealm,
   Realm(..),   Realmable(..),
   Value(..), DynValue(..)
  ) where

import Data.Dynamic
import qualified Data.Graph.Inductive as G
import NumericPrelude


data TGlobal = TGlobal
data TLocal = TLocal
class TRealm a
instance TRealm TGlobal
instance TRealm TLocal

data Realm = Global | Local  deriving (Eq, Show)

class Realmable a where
  realm :: a -> Realm
  
instance Realmable TGlobal where
  realm _ = Global
instance Realmable TLocal where
  realm _ = Local
instance Realmable Realm where
  realm = id  
  
data (TRealm rea, Typeable content) => 
  Value rea content = FromNode rea G.Node | Imm rea content  deriving (Eq, Show)
  
data DynValue = DynValue {rea :: Realm, typeRep :: TypeRep} deriving (Eq, Show)
instance Realmable DynValue where
  realm = rea