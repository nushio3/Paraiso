{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Data.Traversable
import Language.Paraiso.Tensor


type Vec5 = (:~) Vec4

v5 :: Vec5 String
v5 = Vec :~ "funf" :~ "vier" :~ "drei" :~ "zwei" :~ "funf"

t :: Vec3 (Vec3 Int)

numberer :: Int -> String -> (Int, (Int,String))
numberer i str = (i+1, (i,str))

main :: IO ()
main = do
  print $ v5
  print $ snd $ mapAccumL numberer 1 v5
  print $ snd $ mapAccumR numberer 1 v5
  return ()
