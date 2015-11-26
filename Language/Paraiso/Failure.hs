{-# OPTIONS -Wall #-}

-- | a module for handling failure
module Language.Paraiso.Failure
    (
     module Control.Failure, unsafePerformFailure
    ) where

import Control.Failure
import System.IO.Unsafe

unsafePerformFailure :: IO a -> a
unsafePerformFailure = unsafePerformIO

