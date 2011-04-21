{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}

import Language.Paraiso.Tensor
import Language.Paraiso.PiSystem
import Language.Paraiso.Interval
import Language.Paraiso.Orthotope

import Prelude hiding (null)


main :: IO ()
main = do
  putStrLn "hi!"
  examine $ intersection box box2
  examine $ intersection box3 box4
  where
    box, box2 :: Orthotope3 Int
    box = Vec :~ Interval 42 43 :~ Interval 42 100 :~ Interval 42 300
    box2 = Vec :~ Interval 40 43 :~ Interval 2 43 :~ Interval (-1) 43

    box3, box4 :: Orthotope2 Int
    box3 = Vec :~ Interval 1 10 :~ Interval 1 2
    box4 = Vec :~ Interval 1 10 :~ Interval 3 4

examine :: (PiSystem a, Show a) => a -> IO ()
examine t = do
  print t
  putStrLn $ if null t then "is empty" else "is not empty"
