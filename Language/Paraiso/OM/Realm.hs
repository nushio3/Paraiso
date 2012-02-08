{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | The 'Realm' represents how the data reside in Orthotope Machines. 
-- 'Local' data are n-dimensional array that is distributed among nodes.
-- 'Global' data are single-point value, possibly reside in the master node.

module Language.Paraiso.OM.Realm
  (
   TGlobal(..), TLocal(..), TRealm(..),
   Realm(..),   Realmable(..),
  ) where

-- | Type-level representations
class TRealm a where
  tRealm :: a -> Realm
  unitTRealm :: a
  
data TGlobal = TGlobal
data TLocal = TLocal
instance TRealm TGlobal where
  tRealm _ = Global 
  unitTRealm = TGlobal
instance TRealm TLocal where
  tRealm _ = Local
  unitTRealm = TLocal

-- | Value-level representations
data Realm = Global | Local  deriving (Eq, Show)

-- | Means of obtaining value-level realm from things
class Realmable a where
  realm :: a -> Realm
  
-- | Realmable instances  
instance Realmable Realm where
  realm = id    
  
  