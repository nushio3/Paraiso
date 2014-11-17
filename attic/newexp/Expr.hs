{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GADTs, ScopedTypeVariables, RankNTypes, StandaloneDeriving, TupleSections #-}
module Expr where

import Control.Applicative
import Data.Dynamic
import Data.List
import Data.Ratio
import Text.Printf

type VarName = String

-- Reserved symbols and operator names
data Symbol = Partial | Pair 
  deriving (Eq, Ord, Show, Read)            
data Op1Symbol = Abs | Signum | Paren
  deriving (Eq, Ord, Show, Read)            
data Op2Symbol = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show, Read)            

-- Three-dimensional tensor algebra
data Axis = X | Y | Z
  deriving (Eq, Ord, Typeable)

axes :: [Expr Axis]
axes = [Static "x" X, Static "y" Y, Static "z" Z]

instance Show Axis where
  show X = "x"
  show Y = "y"
  show Z = "z"

type Pt = Axis -> Rational


-- The type for staged expression
data Expr a where
  -- An atomic expr with given name
  Var :: VarName -> Expr a
  -- An expr whose actual value is known (also its string expression)
  Static :: String -> a -> Expr a
  -- Reserved Symbols
  Reserved :: Symbol -> Expr a
  -- Operators that encode closedness of algebras
  Op1 :: Op1Symbol -> (a->a) -> Expr a -> Expr a
  Op2 :: Op2Symbol -> (a->a->a) -> Expr a -> Expr a -> Expr a
  -- Generic Functional Application
  (:$) :: (Typeable b) => Expr (b->a) -> Expr b -> Expr a
  deriving (Typeable)

infixl 2 :$


debugPrint :: Typeable a => Expr a -> String
debugPrint x = go x
  where
    go :: forall a. Typeable a => Expr a -> String
    go x@(Var n) = printf "var(%s :: %s)" n  (show $ typeOf x)
    go (Static s x) = printf "static(%s :: %s)" s (show $ typeOf x)
    go (Reserved s) = printf "reserved(%s)" $ show s
    go (Op1 o _ a) = printf "%s(%s)" (show o) (go a)
    go (Op2 o _ a b) = printf "(%s `%s` %s)" (go a) (show o) (go b)
    go (f :$ a) = printf "%s(%s)" (go f) (go a)


-- The type for statement
infix 1 :=
data Stmt a where
  (:=) :: Expr a -> Expr a -> Stmt a 

instance Show (Stmt a) where
  show (a := b) = show a ++ " = " ++ show b


--- alternative call with type-safe cast
(|||?) :: (Typeable a,Typeable c) => (c->b)->(a->b)->(a->b)
(|||?) g f x = case cast x of
  Just x' -> g x'
  _       -> f x

--- type-specific modification function with type-safe cast
(%|||?) :: (Typeable a,Typeable b) => (b->b) -> (a->a) -> (a->a)
(%|||?) g f x = case cast x of
  Just x' -> case cast (g x') of
    Just y -> y
    _      -> f x
  _       -> f x

--- type-specific modification with type-safe cast
(%?) :: (Typeable a,Typeable b) => (b->b) -> (a->a)
(%?) g = g %|||? id

--- type-specific overwrite with type-safe cast
(?=) :: (Typeable a,Typeable b) => a -> b ->a
(?=) a b = case cast b of 
             Just a' -> a'
             Nothing -> a


infixr 9 |||? 
infixr 9 %|||? 
infixr 9 %? 
infixl 9 ?=

------------------------------------------------
-- Algebraic instances for Expr
------------------------------------------------
instance (Typeable a) => Eq (Expr a) where
  Var a       == Var b        = a==b
  Static a _  == Static b _   = a==b
  Reserved a  == Reserved b   = a==b
  Op1 o _ a   == Op1 p _ b    = (o,a) == (p,b)
  Op2 o _ a c == Op2 p _ b d  = (o,a,c) == (p,b,d)
  (f :$ a)    == (g :$ b)     = (Just f,Just a)==(cast g,cast b)
  _ == _ = False

instance Num a => Num (b->a) where
  a+b = (\x -> a x + b x)
  a-b = (\x -> a x - b x)
  a*b = (\x -> a x * b x)
  fromInteger = const . fromInteger
  abs = (abs .)
  signum = (signum .)

instance (Typeable a, Num a) => Num (Expr a) where
  (+) = Op2 Add (+)
  (-) = Op2 Sub (-)
  (*) = Op2 Mul (*)
  fromInteger x = Static (show x) (fromInteger x)
  abs = Op1 Abs abs
  signum = Op1 Signum signum

instance Fractional a => Fractional (b->a) where
  a/b = (\x -> a x / b x)
  fromRational = const . fromRational

instance (Typeable a, Fractional a) => Fractional (Expr a) where
  (/) = Op2 Div (/)
  fromRational x = Static str (fromRational x) 
    where
      str
        | denominator x == 1 = show (numerator x)
        | otherwise          = printf "\\frac{%s}{%s}" 
           (show $ numerator x) (show $ denominator x)


-- apply replace at all atomic Expr
replaceAtom :: (Typeable a, Typeable b) => Expr b -> Expr b -> Expr a -> Expr a
replaceAtom i1 i2 = go
  where
    go :: Typeable a => Expr a -> Expr a
    go (f :$ a)      = go f :$ go a
    go (Op1 o f a)   = Op1 o f (go a)
    go (Op2 o f a b) = Op2 o f (go a) (go b)
    go x             = (\i -> if i==i1 then i2 else i) %?  x



------------------------------------------------
-- Pretty printer for Expr
------------------------------------------------

ppr :: Expr a -> String
ppr = pprRaw . insertParen

pprRaw :: forall a. Expr a -> String
pprRaw x = case x of
  (Var x) -> x
  (Op1 Paren _ a) -> printf "\\left(%s\\right)" (pprRaw a)
  (Op1 o _ a) -> printf "%s\\left(%s\\right)" (show o) (pprRaw a)
  (Op2 Add _ a b) -> printf "%s + %s" (pprRaw a) (pprRaw b)
  (Op2 Sub _ a b) -> printf "%s - %s" (pprRaw a) (pprRaw b)
  (Op2 Mul _ a b) -> printf "%s %s" (pprRaw a) (pprRaw b)
  (Op2 Div _ a b) -> printf "\\frac{%s}{%s}" (pprRaw a) (pprRaw b)
  (Reserved Pair :$ a :$ b) -> printf "{%s,%s}" (pprRaw a) (pprRaw b)
  (Reserved Partial) -> "\\partial"
  (a :$ b) -> 
     let pprAp1 :: Expr Axis -> String
         pprAp1 b' = printf "%s_{%s}" (pprRaw a) (pprRaw b')
         pprAp2 :: Expr (Axis,Axis) -> String
         pprAp2 b' = printf "%s_{%s}" (pprRaw a) (pprRaw b')

         pprApDef :: Expr b -> String
         pprApDef b' = printf "%s\\!\\left(%s\\right)" (pprRaw a) (pprRaw b')
     in pprAp1 |||? pprAp2 |||? pprApDef  $ b
  (Static s _) -> s
  _ -> "?"

instance Show (Expr a) where show = ppr


insertParen :: Expr a -> Expr a
insertParen x@(Op2 o f a b) = Op2 o f a2 b2
  where
    a1 = insertParen a
    b1 = insertParen b
    a2 = if precedence a1 < precedence x then paren a1 else a1
    b2 = if precedence b1 < precedence x then paren b1 else b1
insertParen (Op1 o f a) = Op1 o f (insertParen a)
insertParen x@(f :$ a) = f2 :$ insertParen a
  where
    f1 = insertParen f
    f2 = if precedence f1 < precedence x then paren f1 else f1    
insertParen x = x

paren :: Expr a -> Expr a
paren x = Op1 Paren id x

precedence :: Expr a -> Int
precedence (Op2 Add _ _ _) = 6
precedence (Op2 Sub _ _ _) = 6
precedence (Op2 Mul _ _ _) = 7
precedence (Op2 Div _ _ _) = 7
precedence _               = 10




