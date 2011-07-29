{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that sets the execution priority of the 
-- statements. Statements with 'Ballon's will be allocated
-- as fast as possible, and statements with negative ballons, 
-- or @Stone@s, will be allocated as later as possible.

module Language.Paraiso.Annotation.Ballon
    (
     Ballon(..)
    ) where

import Data.Dynamic
import Data.Text (Text)

data (Ord a, Typeable a) => Balloon
    = Balloon a
      deriving (Eq, Ord, Typeable)
