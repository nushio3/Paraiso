{-# OPTIONS -Wall #-}

data Expr = 
    Term String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Inv Expr
  | Neg Expr
    deriving (Eq,Ord,Read,Show)

instance Num Expr where
  a + b = Add a b
  a - b = Sub a b
  a * b = Mul a b
  negate = Neg
  abs = undefined
  signum = undefined
  fromInteger = Term . show
  



expr :: Expr
expr = a + b * c
    where  
      [a,b,c] = map Term $ words "a b c"
      
main :: IO ()
main = do
  print expr