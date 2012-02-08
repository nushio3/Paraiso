{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
-- | An effectless 'Annotation' with a comment 

module Language.Paraiso.Annotation.Execution
    (
      Alive(..)
    ) where

import Data.Dynamic
import Language.Paraiso.Prelude

-- | used in dead code elimination.
data Alive = Alive Bool
             deriving (Eq, Show, Typeable)
