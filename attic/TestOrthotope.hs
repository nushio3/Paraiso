{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}

import Data.Array.Repa((:.)(..))
import Language.Paraiso.Set
import Language.Paraiso.Interval
import Language.Paraiso.Orthotope

main :: IO ()
main = do
  putStrLn "hi!"
  print $ intersection box box2
  where
    box, box2 :: Orthotope3 Int
    box = Z :. Interval 42 43 :. Interval 42 100 :. Interval 42 300
    box2 = Z :. Interval 40 43 :. Interval 2 43 :. Interval (-1) 43