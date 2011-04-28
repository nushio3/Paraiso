{-# OPTIONS -Wall #-}

import Data.Typeable

three :: Integer
three = 3

answer :: Double
answer = 42

f :: Int -> Int
f = (*3)

main :: IO ()
main = do
  putStrLn "hom mud"
  print $ typeOf (True,three,("Hogeee",answer))
  print $ typeOf f
