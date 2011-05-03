{-# OPTIONS -Wall #-}

-- | name identifier.
module Language.Paraiso.Name 
  (
   Name(..),
   Named(..)
  ) where

newtype Name = Name String deriving (Eq, Show)

class Named a where
  name :: a -> Name
  nameStr :: a -> String
  nameStr = (\(Name str) -> str) . name

instance Named Name where
  name = id
