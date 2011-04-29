{-# LANGUAGE NoImplicitPrelude, TypeOperators #-}
{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Traversable
import Language.Paraiso.Tensor
import NumericPrelude



v1 :: Vec1 Int
v1 = Vec :~ 0

v2 :: Vec2  Int
v2 =  Vec :~ 4 :~ 2

v4 :: Vec4 Int
v4 = Vec :~ 1 :~ 3 :~ 4 :~ 1

t4 :: Vec4 (Vec4 Int)
t4 = compose (\i -> compose (\j -> if i==j then 1 else 0))

main :: IO ()
main = do
  print $ v1
  print $ v2
  print $ v4
  _ <- Data.Traversable.mapM print v4
  Control.Monad.forM_  [0..3] (\i-> componentF (Axis i) v4 >>= print)
  bases <- Control.Monad.forM [0..3] (\i-> unitVectorF (Axis i))
  print $ v4:zero:bases
  print $ compose (\i -> compose (\j -> component i v4 * component j v4 ))
  print $ t4
  print $ t4+t4
  return ()
