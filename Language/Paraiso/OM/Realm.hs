{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | The 'Realm' represents how the data reside in Orthotope Machines. 
-- 'Array' data are n-dimensional array that is distributed among nodes.
-- 'Scalar' data are single-point value, possibly reside in the master node.
-- .
-- Be noted that 'Array' and 'Scalar' were initially called 'Local' and 'Global'.
-- but I opted for more conventional notation. If you find any historical notation
-- remaining, please let me know!


module Language.Paraiso.OM.Realm
  (
   TScalar(..), TArray(..), TRealm(..),
   Realm(..),   Realmable(..),
  ) where

-- | Type-level representations of realm
class TRealm a where
  tRealm :: a -> Realm
  unitTRealm :: a
  
-- | Type-level representation of 'Scalar' realm
data TScalar = TScalar
-- | Type-level representation of 'Array' realm
data TArray = TArray

instance TRealm TScalar where
  tRealm _ = Scalar 
  unitTRealm = TScalar
instance TRealm TArray where
  tRealm _ = Array
  unitTRealm = TArray

-- | Value-level representations of realm
data Realm 
  = Scalar -- ^ Value-level representation of 'Scalar' realm
  | Array  -- ^ Value-level representation of 'Array' realm
  deriving (Eq, Show)

-- | Means of obtaining value-level realm from things
class Realmable a where
  realm :: a -> Realm
  
-- | Realmable instances  
instance Realmable Realm where
  realm = id    
instance Realmable TScalar where
  realm = const Scalar
instance Realmable TArray where
  realm = const Array    
  
  