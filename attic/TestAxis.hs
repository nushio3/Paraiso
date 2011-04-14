{-# OPTIONS -Wall #-}

import Control.Monad
import Language.Paraiso.Axis

x :: Vec3 Int
x = Vec :. 3 :. 9 :. 8

z :: Vec2 Double
z = zeroVector

main :: IO ()
main = do
  putStrLn $ "x = " ++ show x
  forM_ [0..dimension x - 1] $ do
         (\n -> putStrLn $ "x[" ++ show n ++ "] = " ++ show (getComponent (Axis n) x))
  print z