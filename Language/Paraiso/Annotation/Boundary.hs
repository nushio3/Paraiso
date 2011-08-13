{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, NoImplicitPrelude, StandaloneDeriving, 
UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | calculate the 'Valid' regions of the 'Orthotope' where all information needed to update 
--   the region is available.

module Language.Paraiso.Annotation.Boundary
    (
     Valid(..)
    ) where

import Data.Dynamic
import Language.Paraiso.Interval
import Language.Paraiso.Prelude
import Language.Paraiso.PiSystem
import Language.Paraiso.Tensor


data Valid a = Valid a deriving (Eq, Show, Typeable)
               

