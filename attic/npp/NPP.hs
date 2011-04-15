{-# OPTIONS -Wall #-}

module NPP
    (
     Expr(..), optimize, gen,
     SSE2v2r8(..)
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
  a / b = Mul a (Inv b) -- Div a b
  recip = Inv
  fromRational x = let 
        dx :: Double
        dx = fromRational x
   in s2v dx
  
optimize :: Expr -> Expr
optimize expr = let o = optimize in case expr of
  Add (Div a b) c -> Madd (o a) (Inv $ o b) (o c)
  Add c (Div a b) -> Madd (o a) (Inv $ o b) (o c)
  Sub (Div a b) c -> Msub (o a) (Inv $ o b) (o c)
  Sub c (Div a b) -> Nmsub (o a) (Inv $ o b) (o c)
  Add (Mul a b) c -> Madd (o a) (o b) (o c)
  Add c (Mul a b) -> Madd (o a) (o b) (o c)
  Sub (Mul a b) c -> Msub (o a) (o b) (o c)
  Sub c (Mul a b) -> Nmsub (o a) (o b) (o c)
  -- recursive optimization --
  t@(Term _)  -> t
  Add a b     -> Add (o a) (o b)        
  Mul a b     -> Mul (o a) (o b)        
  Sub a b     -> Sub (o a) (o b)        
  Div a b     -> Div (o a) (o b)        
  Inv a       -> Inv (o a)              
  Neg a       -> Neg (o a)              
  Madd  a b c -> Madd  (o a) (o b) (o c)
  Msub  a b c -> Msub  (o a) (o b) (o c)
  Nmadd a b c -> Nmadd (o a) (o b) (o c)
  Nmsub a b c -> Nmsub (o a) (o b) (o c)

class InstructionSet a where
  mnemonic :: a -> Expr -> String
            
gen :: (InstructionSet is) => is -> Expr -> String
gen is expr = 
  case expr of
    Term s -> s
    Add a b -> f2 a b
    Mul a b -> f2 a b
    Sub a b -> f2 a b
    Div a b -> f2 a b
    Inv a -> f1 a
    Neg a -> f1 a
    Madd  a b c -> f3 a b c
    Msub  a b c -> f3 a b c
    Nmadd a b c -> f3 a b c
    Nmsub a b c -> f3 a b c
    where
      g = gen is
      f1 a = (mnemonic is expr) ++ "(" ++ g a ++ ")"
      f2 a b = (mnemonic is expr) ++ "(" ++ g a ++ "," ++ g b ++ ")"
      f3 a b c = (mnemonic is expr) ++ "(" ++ g a ++ "," ++ g b ++ "," ++ g c ++ ")"

data SSE2v2r8 = SSE2v2r8
instance InstructionSet SSE2v2r8 where
    mnemonic _ expr = case expr of
      Term _ -> error "term has no mnemonic"
      Add _ _ -> "v2r8_add"
      Mul _ _ -> "v2r8_mul"
      Sub _ _ -> "v2r8_sub"
      Div _ _ -> error "division undefined for SSE2v2r8"
      Inv _ -> "v2r8_inv"
      Neg _ -> "v2r8_neg"
      Madd _ _ _ -> "v2r8_madd"
      Msub _ _ _ -> "v2r8_msub"
      Nmadd _ _ _ -> "v2r8_nmadd"
      Nmsub _ _ _ -> "v2r8_nmsub"

      