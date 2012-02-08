{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that sets the execution priority of the 
-- statements. Statements with 'Ballon's will be allocated
-- as fast as possible, and statements with negative ballons, 
-- or @Stone@s, will be allocated as later as possible.

module Language.Paraiso.Annotation.Balloon (
  Balloon(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Prelude
import Prelude (Eq, Ord)

data (Ord a, Typeable a) => Balloon a
    = Balloon a
      deriving (Eq, Ord, Typeable)
