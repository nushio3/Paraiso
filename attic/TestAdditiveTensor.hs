{-# LANGUAGE TypeOperators, NoImplicitPrelude  #-}
{-# OPTIONS -Wall #-}

import Algebra.Additive as Additive
import Data.Foldable
import Control.Monad
import Data.Traversable
import Language.Paraiso.Tensor
import NumericPrelude



infixl 9 !
(!) :: Vector v => v a -> Axis v -> a
v ! i  = component i v


a :: Vec3 Int
a = Vec :~ 2 :~ 3 :~ 5

ex, ey, ez :: Vec3 Int
ex = unitVector $ Axis 0
ey = unitVector $ Axis 1
ez = unitVector $ Axis 2

-- levi-civita sybmol
eps :: Vec3 (Vec3 (Vec3 Int))
eps = compose (\(Axis i) -> compose (\(Axis j) -> compose (\(Axis k) -> (i-j)*(j-k)*(k-i) `div` 2)))




main :: IO ()
main = do
  print eps
  putStrLn ""
  print a
--  print $ Data.Foldable.foldl (+) Additive.zero $  compose (\i-> component i a)
  print $ contract (\i -> a!i)
  return ()

{-  
print $ compose (\i -> contract (\j -> contract(\k -> 
    eps!i!j!k * ex!j * a!k )))
-}
