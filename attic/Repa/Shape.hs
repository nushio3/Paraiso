#!/usr/bin/env runhaskell
{-# Language FlexibleContexts, FlexibleInstances, FunctionalDependencies, 
MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as R
import           Language.Paraiso.Tensor


t3 :: (:~) ((:~) Vec) :~ Int
t3 = Vec :~ 13 :~ 41 :~ 398

sh :: R.Z :. Int :. Int :. Int
sh = R.shapeOfList [398, 41, 13]

class Interpretable a b | a -> b where
  interpret :: a -> b

instance Interpretable (Vec Int) (R.Z) where
  interpret Vec = R.Z
  
instance Interpretable (vec Int) sh => Interpretable (vec :~ Int) (sh :. Int) where
  interpret (xs :~ x) = (interpret xs) :. x


main :: IO ()
main = do
  print $ R.Z :. (13::Int) :. (41::Int) :. (398::Int)
  print t3
  putStrLn $ R.showShape $ sh
  putStrLn $ R.showShape $ interpret t3