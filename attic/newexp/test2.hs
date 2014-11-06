{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}

import qualified Data.Vector as V
import Data.Ratio
import Data.List
import System.Process
import Text.Printf

mkAxis :: VarName -> Expr Axis
mkAxis = AVar

type FunExpr a = [Expr Axis] -> Expr a

mkF :: VarName -> FunExpr a
mkF fn ixs = At  ixs (Var fn)

mk2F :: VarName -> [Expr Axis] -> Expr a -> Expr a
mk2F fn ixs = Apply (mkF fn ixs) 


main = do
  let i = mkAxis "i"
      j = mkAxis "j"
      k = mkAxis "k"

      f :: FunExpr S
      f =  mkF "f"
      σ :: FunExpr S
      σ = mkF "\\sigma"

      δ :: FunExpr S
      δ = mkF "\\delta"

      v :: FunExpr S
      v = mkF "v"

      λ :: Expr S
      λ = Var "\\lambda"
      μ :: Expr S
      μ = Var "\\mu"

      ә :: [Expr Axis] -> Expr a -> Expr a
      ә = mk2F "\\partial"

      әt :: Expr a -> Expr a
      әt = Apply $ Var "\\partial_t"

      p :: Expr a -> Expr a
      p = Apply $ Var ""


      eqV =     әt(v[i])   := ә[j] (σ[i,j]) + f[i]
      eqS =     әt(σ[i,j]) := μ * p(ә[i](v[j]) + ә[j](v[i]))
                            + λ *  (δ[i,j] * ә[k](v[k]))



  let progStr :: String 
      progStr = 
        intercalate "\\\\" $ 
        map (++"\\nonumber") $
        map show $ concatMap einsteinRule $ 
        [eqV, eqS]

  writeFile "tmp.tex" $ printf 
    "\\documentclass[9pt]{article}\\begin{document}\\begin{eqnarray}%s\\end{eqnarray}\\end{document}" progStr
  system "pdflatex tmp.tex"
  return ()


type VarName = String

data T x = T x  deriving (Eq, Show) -- tensor
data S = S deriving (Eq, Show) -- scalar
data Axis = Axis   deriving (Eq, Show)


infix 1 :=
data Stmt a where
  (:=) :: Expr a -> Expr a -> Stmt a 

instance Show (Stmt a) where
  show (a := b) = show a ++ " &=& " ++ show b

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
  show (At ixs a) = show a ++ "_{" ++ (intercalate "," $ map show ixs) ++ "}"
  show (Apply f a) = show f ++ "\\left(" ++ show a ++ "\\right)"

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



einsteinRule :: Stmt a -> [Stmt a]
einsteinRule (lhs := rhs) = ret
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

