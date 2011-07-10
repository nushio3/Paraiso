{-# LANGUAGE TypeOperators, NoImplicitPrelude  #-}
{-# OPTIONS -Wall #-}

import qualified Algebra.Additive as Additive
import Language.Paraiso.Tensor
import NumericPrelude

type Vec6 = (:~) ((:~) Vec4)
type Vec9 = (:~) ((:~) ((:~) Vec6))

c_ :: Vector v => (Axis v -> a) -> v a
c_ = compose

s_ :: (Vector v, Additive.C a) => (Axis v -> a) -> a
s_ = contract


norm :: Vec9 Double -> Double
norm a = s_ (\i -> a!i * a!i)

abso :: Vec9 Double -> Double
abso = sqrt . norm

unitize :: Vec9 Double -> Vec9 Double
unitize a = let z = 1/abso a in
            c_ (\i -> z * a!i)

density' :: Vec9 Double
density' = Vec :~ 1 :~ 1 :~ 1 :~ 1 :~ 1 :~ 1 :~ 1 :~ 1 :~ 1

density :: Vec9 Double
density = unitize density'

momx' :: Vec9 Double
momx' = Vec :~ (-1) :~ 0 :~ 1 :~ (-1) :~ 0 :~ 1 :~ (-1) :~ 0 :~ 1

momx :: Vec9 Double
momx = unitize momx'

momy' :: Vec9 Double
momy' = Vec :~ (-1) :~ (-1) :~ (-1) :~ 0 :~ 0 :~ 0 :~ 1 :~ 1 :~ 1

momy :: Vec9 Double
momy = unitize momy'

energy' :: Vec9 Double
energy' = Vec :~ 2 :~ (-1) :~ 2 :~ (-1) :~ (-4) :~ (-1) :~ 2 :~ (-1) :~ 2

energy :: Vec9 Double
energy = unitize energy'


vecs :: [Vec9 Double]
vecs = [density,momx,momy,energy]

-- Kronecker's Delta
del :: Vec9 (Vec9 Double)
del = compose (\i -> compose (\j -> if i==j then 1 else 0))

main :: IO ()
main = do
  putStrLn "density"
  print $ [s_ (\i -> a!i * b!i)  |a <- vecs, b <- vecs]


