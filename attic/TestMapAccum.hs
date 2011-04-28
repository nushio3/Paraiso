{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Data.Traversable
import Language.Paraiso.Tensor


type Vec5 = (:~) Vec4

v5 :: Vec5 String
v5 = Vec :~ "funf" :~ "vier" :~ "drei" :~ "zwei" :~ "ein"

japa :: [String]
japa = words "ichi ni san shi"


t42 :: Vec4 (Vec2 String)
t42 = compose (\(Axis i)-> compose (\(Axis j) -> japa!!i ++ "_" ++ japa!!j))

f :: Int -> String -> (Int, (Int,String))
f i str = (i+1, (i,str))

main :: IO ()
main = do
  print $ v5
  print $ snd $ mapAccumL f 1 v5
  print $ snd $ mapAccumR f 1 v5
  print t42
  print $ snd $ mapAccumL (mapAccumL f) 1 t42
  return ()
