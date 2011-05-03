{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | a general code generator definition.
module Language.Paraiso.Generator
    (
     Generator(..), Symbolable(..)
    ) where

import qualified Algebra.Ring as Ring
import Language.Paraiso.Failure
import Language.Paraiso.POM
import Language.Paraiso.Tensor (Vector)

-- | The definition for code generator.
class Generator gen where
    generate :: (Vector v, Ring.C g) =>
                gen           -- ^The code generator.
             -> POM v g a     -- ^The 'POM' to be translated.
             -> FilePath      -- ^The directory name under which the files are to be generated.
             -> IO ()         -- ^The act of generation.

-- | The translation of Haskell symbols to other languages.
class (Generator gen) => Symbolable gen a where
    -- | Failure handling version of symbol translation.
    symbolF :: (Failure StringException f) =>
               gen      -- ^The 'Generator'.
            -> a        -- ^A Haskell object to be translated.
            -> f String -- ^The translation result which may fail.
    -- | Pure version, which may emit runtime error.
    symbol :: gen -> a -> String
    symbol gen0 a0 = unsafePerformFailure (symbolF gen0 a0)
