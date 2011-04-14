{-# OPTIONS -Wall #-}

import Language.Paraiso.Axis

x :: Vec2 Int
x = Vec :. 4 :. 2

main :: IO ()
main = do
  putStrLn $ "x = " ++ show x
  