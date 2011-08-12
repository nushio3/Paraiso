#!/usr/bin/env runhaskell
{-# Language FlexibleContexts, FlexibleInstances, FunctionalDependencies, 
MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as R

sh :: R.DIM2
sh = R.Z :. 10 :. 20

ar :: R.Array R.DIM2 Int
ar = R.fromFunction sh (const undefined)

main :: IO ()
main = do
  putStrLn $ R.showShape sh
  print $ R.map (const (1::Int)) ar
