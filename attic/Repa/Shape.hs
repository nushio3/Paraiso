#!/usr/bin/env runhaskell
{-# Language TypeOperators #-}
{-# OPTIONS -Wall #-}

import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as R


sh :: R.Z :. Int :. Int :. Int
sh = R.shapeOfList [13,41,398]

main :: IO ()
main = do
  print $ R.Z :. (13::Int) :. (41::Int) :. (398::Int)
  putStrLn $ R.showShape $ sh