{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, 
UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | calculate the 'Valid' regions for each 'Orthotope' value 
--   where all information needed to update 
--   the region is available.
--   also annotates the global boundary condition.

module Language.Paraiso.Annotation.Boundary
    (
     Valid(..), NearBoundary(..), Condition(..)
    ) where

import Data.Dynamic
import Language.Paraiso.Prelude
import Language.Paraiso.Interval
import Language.Paraiso.PiSystem

-- | a type that represents valid region of computation.
newtype Valid g = Valid [Interval (NearBoundary g)] deriving (Eq, Show, Typeable)
               
instance (Ord g) => PiSystem (Valid g) where                                                            
  empty = error "empty set is undefined for type Valid"
  null = const False
  intersection (Valid xs) (Valid ys)
    | length xs /= length ys = error "length mismatch in merging two Valid"
    | otherwise              = Valid $ zipWith intersection xs ys
                                                             
-- | the displacement around either side of the boundary.
data NearBoundary a = NegaInfinity | LowerBoundary a | UpperBoundary a | PosiInfinity
              deriving (Eq, Ord, Show, Typeable)

-- | type for global boundary conditions                       
data Condition
     = Open   -- ^ open boundary; do not touch anything
     | Cyclic -- ^ cyclic boundary; data out of the bounds are copied from the other side
     deriving (Eq, Read, Show)