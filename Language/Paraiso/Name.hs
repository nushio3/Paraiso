{-# Language StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | name identifier.
module Language.Paraiso.Name 
  (
   Name(..), Named(..),
   Nameable(..)
  ) where

-- | a name.
newtype Name = Name String deriving (Eq, Ord, Show, Read)

-- | something that has name.
class Nameable a where
  -- | get its name.
  name :: a -> Name
  -- | get its name as a 'String'.
  nameStr :: a -> String
  nameStr = (\(Name str) -> str) . name

-- | 'Name' has 'Name'. 'Name' of 'Name' is 'Name' itself. 
instance Nameable Name where
  name = id

-- | give something a name.
data Named a = Named Name a
instance Nameable (Named a) where
  name (Named n _) = n

deriving instance (Eq a) => Eq (Named a)
deriving instance (Ord a) => Ord (Named a)
deriving instance (Show a) => Show (Named a)
deriving instance (Read a) => Read (Named a)
