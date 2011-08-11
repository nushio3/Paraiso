{-# OPTIONS -Wall #-}

-- | Paraiso main module.
-- This module will export a starter-kit modules and functions in the future,
-- but is useless right now.

module Language.Paraiso (run) where

-- | Generate Wonderful Program
run :: () -> String
run _ = "#include <iostream>\nint main () {cout << \"hello\" << endl;}\n"
