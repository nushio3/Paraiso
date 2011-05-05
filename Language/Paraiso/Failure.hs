{-# OPTIONS -Wall #-}

-- | a module for handling failure
module Language.Paraiso.Failure
    (
     module Control.Monad.Failure, unsafePerformFailure
    ) where

import Control.Monad.Failure
import System.IO.Unsafe

unsafePerformFailure :: IO a -> a
unsafePerformFailure = unsafePerformIO

