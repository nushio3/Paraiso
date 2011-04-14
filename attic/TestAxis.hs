{-# OPTIONS -Wall #-}

import Control.Monad
import Language.Paraiso.Axis

x :: Vec3 Int
x = Vec :. 3 :. 9 :. 8

z :: Vec3 Double
z = zeroVector

main :: IO ()
main = do
  putStrLn $ "x = " ++ show x
  print z
  forM_ (map Axis [0..dimension x - 1]) sub
  
  
sub :: Axis -> IO ()
sub ex = do
  putStrLn $ "x[" ++ show ex ++ "] = " ++ show (getComponent ex x)
  print $ (unitVector ex::Vec3 Double)
         