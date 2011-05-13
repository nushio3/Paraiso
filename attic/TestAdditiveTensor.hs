{-# LANGUAGE TypeOperators, NoImplicitPrelude  #-}
{-# OPTIONS -Wall #-}

import qualified Algebra.Additive as Additive
import Language.Paraiso.Tensor
import NumericPrelude
import qualified Prelude as P (Num(..)) 

instance P.Num (Axis v) where
    fromInteger = Axis . P.fromInteger

c_ :: Vector v => (Axis v -> a) -> v a
c_ = compose

s_ :: (Vector v, Additive.C a) => (Axis v -> a) -> a
s_ = contract



a :: Vec3 Int
a = Vec :~ 2 :~ 3 :~ 5

ex, ey, ez :: Vec3 Int
ex = unitVector $ Axis 0
ey = unitVector $ Axis 1
ez = unitVector $ Axis 2

-- levi-civita sybmol
eps :: Vec3 (Vec3 (Vec3 Int))
eps = compose (\(Axis i) -> compose (\(Axis j) -> compose (\(Axis k) -> (i-j)*(j-k)*(k-i) `div` 2)))


-- Kronecker's Delta
del :: Vec3 (Vec3 Int)
del = compose (\i -> compose (\j -> if i==j then 1 else 0))

main :: IO ()
main = do
  putStrLn "Levi Civita tensor for 3-dimension"
  print eps
  putStrLn "a vector"
  print a

  putStrLn "testing for outer products"
  print $ c_(\i -> s_(\j -> s_(\k -> 
    eps!i!j!k * ex!j * a!k )))
  print $ c_(\i -> s_(\j -> s_(\k -> 
    eps!i!j!k * ey!j * a!k )))
  print $ c_(\i -> s_(\j -> s_(\k -> 
    eps!i!j!k * ez!j * a!k )))
  
  putStrLn "testing for theorems on Levi Civita tensor"
  print $ s_(\i -> s_(\j -> s_(\k -> eps!i!j!k * eps!i!j!k))) == 6
  print $ c_(\i -> c_(\j -> s_(\m -> s_(\n -> eps!i!m!n * eps!j!m!n)))) == 
    c_(\i -> c_(\j -> 2 * del!i!j))
  print $ c_(\i -> c_(\j -> s_(\k -> c_(\l -> c_(\m -> eps!i!j!k * eps!k!l!m))))) ==
    c_(\i -> c_(\j -> c_(\l -> c_(\m -> del!i!l * del!j!m - del!i!m * del!j!l))))
