{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
-- | An 'Annotation' that lets you call __syncthreads() before
-- or after a statement.

module Language.Paraiso.Annotation.SyncThreads (
  Timing(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Prelude

data Timing = Pre | Post
      deriving (Eq, Ord, Typeable)
