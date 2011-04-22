{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Language.Paraiso.Tensor


infixl 2 :&
data (c1 :& c2) a = c1 a :& c2 a

s :: Int
s = 4

v :: Vec4 Int
v = unitVector (Axis 0)

t :: Vec4 (Vec4 Int)
t = compose (\i -> compose (\j -> if i==j then 1 else 0))


svt = s :& v :& t

main :: IO ()
main = do
  print s
  print v
  print t
  putStrLn "Hi"

