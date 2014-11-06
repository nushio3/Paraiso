{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}

import qualified Data.Vector as V
import Data.Ratio
import Data.List
import System.Process
import Text.Printf
import Linear

type Pt r = V3 r
type PtR = Pt Rational
data Axis = X | Y | Z deriving (Eq, Ord)

instance Show Axis where
  show X = "x"
  show Y = "y"
  show Z = "z"

type Field r a = Pt r -> a
type Tensor1 a = Axis -> a
type Tensor2 a = (Axis,Axis) -> a


asT1 :: V3 a -> Tensor1 a
asT1 (V3 x _ _) X = x
asT1 (V3 _ y _) Y = y
asT1 (V3 _ _ z) Z = z

asT2 :: V3 (V3 a) -> Tensor2 a
asT2 = uncurry . fmap asT1 . asT1 

sigma :: (Show r) => Tensor2 (Field r String)
sigma (a,b) r = printf "\\sigma_{%s,%s}[%s]" (show $ min a b) (show $ max a b) (show r)

partial :: forall a r. (Fractional a, Fractional r) => Tensor1 (Field r a -> Field r a)
partial i f p = (f (p + 0.5 *^ e(i)) - f (p - 0.5 *^ e(i))) * (fromRational $ 9%8)
              - (f (p + 1.5 *^ e(i)) - f (p - 1.5 *^ e(i))) * (fromRational $ 1%24)
  where
    e :: Tensor1 (Pt r)
    e = asT1 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) 

p2 :: Expr a -> Expr a
p2 (Var x :$ y :$ z) = --rewrite rules
p2 x = x


-- The variables and the operators
type VarName = String
data Op2 = Add | Mul | Sub 
instance Show Op2 where
  show Add = "+"         
  show Mul = " "         
  show Sub = "-"         

-- The Expression
data Expr a where
  Var :: VarName -> Expr x
  AVar :: VarName -> Expr Axis
--  Static :: use partial evalutation.
  Imm :: Double -> Expr x
  AImm :: Axis -> Expr Axis
  Expr2 :: Op2 -> Expr x -> Expr x -> Expr x
  (:$) :: Expr (a->b) -> Expr a -> Expr b
infixl 9 :$

-- The Statement
infix 1 :=
data Stmt a where
  (:=) :: Expr a -> Expr a -> Stmt a 

instance Show (Stmt a) where
  show (a := b) = show a ++ " &=& " ++ show b


--  V3 in expression level
v3e ::  Expr a ->  Expr a ->  Expr a ->  Expr (V3 a)
v3e a b c = Var "V3" :$ a :$ b :$ c


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
  show (f :$ a) = show f ++ "\\left(" ++ show a ++ "\\right)"

-- todo: use syb
indicesIn :: Expr a -> [VarName]
indicesIn (AVar n) = [n]
indicesIn (Expr2 _ a b) = nub $ sort $ indicesIn a ++ indicesIn b
indicesIn (f :$ a) = nub $ sort $ indicesIn f ++ indicesIn a
indicesIn _ = []

replaceI :: VarName -> VarName -> Expr a -> Expr a
replaceI i1 i2 = go
  where
    go :: Expr a -> Expr a
    go (AVar i)
      | i == i1 = AVar i2
      | True    = AVar i
    go (Expr2 op a b) = Expr2 op (go a) (go b)
    go (f :$ a) = (go f) :$ (go a)
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



mkAxis :: VarName -> Expr Axis
mkAxis = AVar

type FunExpr a = Expr Axis -> Expr a

mkF :: VarName -> FunExpr a
mkF fn ixs = (Var fn) :$ ixs

mk2F :: VarName -> [Expr Axis] -> Expr a -> Expr a
mk2F fn ixs = (:$) (mkF fn ixs) 





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
      әt = (:$) $ Var "\\partial_t"

      p :: Expr a -> Expr a
      p = (:$) $ Var ""


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
