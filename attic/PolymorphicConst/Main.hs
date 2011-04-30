{-# OPTIONS -Wall #-}

c :: Num a => a
c = 40

y :: Int
y = 2

data Z = Z String Integer deriving (Eq, Show)
instance Num Z where
  Z s1 x1 + Z s2 x2 = Z (s1++s2) (x1+x2)
  fromInteger i = Z "ans" i

z :: Z
z = Z "wer" 2

main :: IO ()
main = do
  putStrLn $ "hi"
  print $ c+y
  print $ c+z