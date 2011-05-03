{-# OPTIONS -Wall #-}

-- | name identifier.
module Language.Paraiso.Name 
  (
   Name(..),
   Nameable(..)
  ) where

newtype Name = Name String deriving (Eq, Show)

class Nameable a where
  name :: a -> Name
  nameStr :: a -> String
  nameStr = (\(Name str) -> str) . name

instance Nameable Name where
  name = id
