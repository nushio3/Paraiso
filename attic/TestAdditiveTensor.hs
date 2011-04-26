{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Traversable
import Language.Paraiso.Tensor



a :: Vec3 Int
a = Vec :~ 2 :~ 3 :~ 5

-- levi-civita sybmol
e :: Vec3 (Vec3 (Vec3 Int))
e = compose (\(Axis i) -> compose (\(Axis j) -> compose (\(Axis k) -> (i-j)*(j-k)*(k-i) `div` 2)))


main :: IO ()
main = do
  print $e
  return ()
