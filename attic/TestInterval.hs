{-# OPTIONS -Wall #-}

import Language.Paraiso.Tensor
import Language.Paraiso.PiSystem
import Language.Paraiso.Interval 

import Prelude hiding (null)

i::Interval Int
i = Interval 2 4

i3 :: Vec3 (Interval Int)
i3 = Vec :~ i :~ i :~ i

main :: IO ()
main = do
  putStrLn "hi!"
  print i
  print i3
  print $ null i3
