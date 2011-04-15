{-# OPTIONS -Wall #-}

module NPP
    (
     Expr(..), gen
    ) where

data Expr = 
    Term String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Inv Expr
  | Neg Expr
  | Madd Expr Expr Expr -- a*b+c
  | Msub Expr Expr Expr -- a*b-c
  | Nmadd Expr Expr Expr -- -a*b-c
  | Nmsub Expr Expr Expr -- -a*b+c
    
    deriving (Eq,Ord,Read,Show)


s2v :: (Show a) => a -> Expr
s2v n = let sn = show n in Term $ "vec_set(" ++ sn ++ "," ++ sn ++ ")"
 

instance Num Expr where
  a + b = Add a b
  a - b = Sub a b
  a * b = Mul a b
  negate = Neg
  abs = undefined
  signum = undefined
  fromInteger = s2v 
  
instance Fractional Expr where
  a / b = Div a b
  recip = Inv
  fromRational = s2v 
  

gen :: Expr -> String
gen expr = 
  case expr of
    Term s -> s
    Add a b -> f2 "add" a b
    Mul a b -> f2 "mul" a b
    Sub a b -> f2 "sub" a b
    Div a b -> f2 "div" a b
    Inv a -> f1 "inv" a
    Neg a -> f1 "neg" a
    Madd  a b c -> f3 "madd"  a b c
    Msub  a b c -> f3 "msub"  a b c
    Nmadd a b c -> f3 "nmadd" a b c
    Nmsub a b c -> f3 "nmsub" a b c
    where
      f1 tag a = tag ++ "(" ++ gen a ++ ")"
      f2 tag a b = tag ++ "(" ++ gen a ++ "," ++ gen b ++ ")"
      f3 tag a b c = tag ++ "(" ++ gen a ++ "," ++ gen b ++ "," ++ gen c ++ ")"

      
