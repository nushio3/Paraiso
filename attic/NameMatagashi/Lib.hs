module Lib(singleton) where

-- singleton borrowed from Set does not cause a problem
import Data.Set 

-- user defined singleton cause an ambiguity problem
-- singleton = id