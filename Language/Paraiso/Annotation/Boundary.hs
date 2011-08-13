{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, NoImplicitPrelude, StandaloneDeriving, 
UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | calculate the 'Valid' regions of the 'Orthotope' where all information needed to update 
--   the region is available.

module Language.Paraiso.Annotation.Boundary
    (
     Valid(..), Pegged(..)
    ) where

import Data.Dynamic
import Language.Paraiso.Prelude

-- | a type that represents valid region of computation.
newtype Valid a = Valid a deriving (Eq, Show, Typeable)
               
-- | the displacement around either side of the boundary.
data Pegged a = LowerBoundary a | UpperBoundary a
              deriving (Eq, Ord, Show, Typeable)
                       
