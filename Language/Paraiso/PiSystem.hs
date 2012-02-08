{-# LANGUAGE CPP, TypeOperators, FlexibleInstances, OverlappingInstances #-}

{- | In mathematics, a pi-system is a non-empty family of sets that is closed
under finite intersections.  -}
module Language.Paraiso.PiSystem (
  PiSystem(..)
  ) where

import qualified Data.Foldable as F
import           Data.Tensor.TypeLevel
import           Prelude hiding (null)

class PiSystem a where
  -- | an empty set.
  empty :: a
  -- | is this an empty set?
  null :: a -> Bool
  -- | intersection of two sets.
  intersection :: a -> a -> a
  
{- | a 'Vector' of 'PiSystem' is also a 'PiSystem'. 
   This is an overlapping instance, 
   can be overwritten by more specific instances.
-}
instance (PiSystem a, Vector v) => PiSystem (v a) where
  empty = compose $ const empty
  null = F.any null
  intersection a b = compose (\i -> component i a `intersection` component i b)
