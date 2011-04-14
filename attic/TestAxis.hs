{-# OPTIONS -Wall #-}

import Language.Paraiso.Axis

x :: Vec3 Int
x = Vec :. 3 :. 9 :. 8

main :: IO ()
main = do
  putStrLn $ "x = " ++ show x
  putStrLn $ "x = " ++ show (getComponent x (Axis 0))
  