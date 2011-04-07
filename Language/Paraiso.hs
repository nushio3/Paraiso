{-# OPTIONS -Wall #-}

module Paraiso (run) where

-- | Generate Wonderful Program
run :: () -> String
run _ = "#include <iostream>\nint main () {cout << \"hello\" << endl;}\n"
