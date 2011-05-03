{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator
    (
     Generator(..), Symbolable(..)
    ) where

import Language.Paraiso.Failure
import Language.Paraiso.POM

class Generator gen where
    generate :: gen -> POM v g a -> FilePath -> IO ()

class (Generator gen) => Symbolable gen a where
    symbolF :: (Failure StringException f) =>
               gen -> a -> f String
    symbol :: gen -> a -> String
    symbol gen0 a0 = unsafePerformFailure (symbolF gen0 a0)