{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}

import qualified Data.Vector as V
import Data.Ratio
import Data.List

main = do
  let i :: Expr Axis
      i = AVar "i"

      j :: Expr Axis
      j = AVar "j"


      f :: Expr (T S)
      f = Var "f"

      σ :: Expr (T S)
      σ = Var "σ"
      v :: Expr (T S)
      v = Var "v"

      ә :: Expr (T (S->S))
      ә = Var "ә"


  print (At [i] f)
  print (At [i,j] σ)
  print $ Apply (At [i] ә) (At [i,j] σ + At [i] f)
  x <- einsteinRule $ At [i] v := Apply (At [j] ә) (At [i,j] σ) + At [i] f
  print x

type VarName = String

data T x = T x  deriving (Eq, Show) -- tensor
data S = S deriving (Eq, Show) -- scalar
data Axis = Axis   deriving (Eq, Show)


infix 1 :=
data Stmt a where
  (:=) :: Expr a -> Expr a -> Stmt a 

instance Show (Stmt a) where
  show (a := b) = show a ++ " := " ++ show b

data Op2 = Add | Mul | Sub 
instance Show Op2 where
  show Add = "+"         
  show Mul = " "         
  show Sub = "-"         

data Expr a where
  Var :: VarName -> Expr x
  AVar :: VarName -> Expr Axis
  Imm :: Double -> Expr x
  Expr2 :: Op2 -> Expr x -> Expr x -> Expr x
  At :: [Expr Axis] -> Expr (T a) -> Expr a
  Apply :: Expr (a->b) -> Expr a -> Expr b

instance Eq (Expr a) where
  AVar x == AVar y = x == y
  _ == _ = False



instance Num (Expr a) where
  a+b = Expr2 Add a b
  a-b = Expr2 Sub a b
  a*b = Expr2 Mul a b
  fromInteger n = Imm (fromInteger n)
  abs = undefined
  signum = undefined

instance Show (Expr a) where
  show (Var n) = n
  show (AVar n) = n
  show (Imm x) = show x
  show (Expr2 o a b) = show a ++ show o ++ show b
  show (At ixs a) = show a ++ show ixs
  show (Apply f a) = show f ++ "(" ++ show a ++ ")"

-- todo: use syb
indicesIn :: Expr a -> [VarName]
indicesIn (AVar n) = [n]
indicesIn (Expr2 _ a b) = nub $ sort $ indicesIn a ++ indicesIn b
indicesIn (At ixs a) =  nub $ sort $ concatMap indicesIn ixs ++ indicesIn a
indicesIn (Apply f a) = nub $ sort $ indicesIn f ++ indicesIn a
indicesIn _ = []

replaceI :: VarName -> VarName -> Expr a -> Expr a
replaceI i1 i2 = go
  where
    go :: Expr a -> Expr a
    go (AVar i)
      | i == i1 = AVar i2
      | True    = AVar i
    go (Expr2 op a b) = Expr2 op (go a) (go b)
    go (At ixs a) =  At (map go ixs) (go a)
    go (Apply f a) = Apply (go f) (go a)
    go x = x



einsteinRule :: Stmt a -> IO [Stmt a]
einsteinRule (lhs := rhs) = do
  print $ lhsi
  print $ rhsi
  print $ freei
  return $ ret
  where
    lhsi = indicesIn lhs
    rhsi = indicesIn rhs

    freei = [i | i <- rhsi , not (i `elem` lhsi)]
    boundi = lhsi

    rhs1 = foldr ($) rhs [byTerms (sumOver j)| j <- freei] 
    
    ret =  foldr (=<<) [lhs := rhs1] [specializeStmt i| i <- boundi] 

-- todo : lens
byTerms :: (forall b. Expr b -> Expr b) -> Expr a -> Expr a
byTerms f (Expr2 Add a b) = Expr2 Add (byTerms f a)(byTerms f b)
byTerms f (Expr2 Sub a b) = Expr2 Sub (byTerms f a)(byTerms f b)
byTerms f x = f x

sumOver :: VarName -> Expr a -> Expr a
sumOver i  expr 
 | i `elem` indicesIn expr = foldr1 (Expr2 Add) [replaceI i j expr | j <- ["x","y","z"]]
 | otherwise = expr

specializeStmt :: VarName -> Stmt a -> [Stmt a]
specializeStmt i (lhs := rhs) = 
  [let f = replaceI i j in f lhs := f rhs| j <- ["x","y","z"]]



{-

ә j f = f[j-1.5*e[j]] + f[j-0.5*e[j]] + f[j+0.5*e[j]]  + f[j+1.5*e[j]]   

  әt(v[i])   ≔ ә[j](σ[i,j]+f[i])
  әt(σ[i,j]) ≔ μ * ә[j](v[j])
             + λ * (δ[i,j] * ә[k](v[k]))

-}

