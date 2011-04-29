{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Data.Foldable as F
import Data.Traversable as D
import Language.Paraiso.Tensor


type Vec5 = (:~) Vec4

v5 :: Vec5 String
v5 = Vec :~ "ein" :~ "zwei" :~ "drei" :~ "vier" :~ "funf"

japa :: [String]
japa = words "ichi ni san shi"


t42 :: Vec4 (Vec2 String)
t42 = compose (\(Axis i)-> compose (\(Axis j) -> japa!!i ++ "_" ++ japa!!j))



t222 :: Vec2 (Vec2 (Vec2 String))
t222 = compose (\(Axis i)-> compose (\(Axis j) -> compose (\(Axis k) -> [c!!i, c!!j, c!!k]) ))
       where c = "BW"

f :: Int -> String -> (Int, (Int,String))
f i str = (i+1, (i,str))


main :: IO ()
main = do
  print $ v5
  print $ snd $ mapAccumL f 1 v5
  print $ snd $ mapAccumR f 1 v5
  print t42
        
        
  let 
    v5i   = snd $ mapAccumL f 1 v5
    t42i  = snd $ mapAccumL (mapAccumL f) 1 t42
    t222i = snd $ mapAccumL (mapAccumL (mapAccumL f)) 1 t222
  print t42i

  print $ foldMap (:[]) v5i
  print $ foldMap (foldMap (:[])) t42i
  print $ foldMap (foldMap (foldMap (:[]))) t222i
  return ()
