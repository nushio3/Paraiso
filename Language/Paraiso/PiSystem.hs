{-# LANGUAGE TypeOperators, FlexibleInstances, OverlappingInstances #-}

module Language.Paraiso.PiSystem (PiSystem(..)) where
{- In mathematics, a pi-system is a non-empty family of sets that is closed
under finite intersections.  -}

import Prelude hiding (null)
import qualified Data.Foldable as F
import Language.Paraiso.Tensor

class PiSystem a where
  empty :: a
  null :: a -> Bool
  intersection :: a -> a -> a
  
instance (PiSystem a, Vector v) => PiSystem (v a) where
  empty = compose $ const empty
  null = F.any null
  intersection a b = compose (\i -> component i a `intersection` component i b)
                                    
