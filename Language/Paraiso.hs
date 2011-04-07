{-# OPTIONS -Wall #-}

-- | Paraiso main module.
-- I think adding this will expose source for run?

module Language.Paraiso (run) where

-- | Generate Wonderful Program
run :: () -> String
run _ = "#include <iostream>\nint main () {cout << \"hello\" << endl;}\n"
