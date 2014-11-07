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
data Op1Symbol = Abs | Signum
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
  -- Operators encode closedness of algebras
  Op1 :: Op1Symbol -> Expr a -> Expr a
  Op2 :: Op2Symbol -> Expr a -> Expr a -> Expr a
  -- Generic Functional Application
  (:$) :: (Typeable b) => Expr (b->a) -> Expr b -> Expr a
  deriving (Typeable)

infixl 2 :$

-- staged computation
runStatic :: Expr a -> Maybe a
runStatic (Static _ x) = Just x
runStatic _            = Nothing

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
  Var a      == Var b      = a==b
  Static a _ == Static b _ = a==b
  Reserved a == Reserved b = a==b
  Op1 o a    == Op1 p b    = (o,a) == (p,b)
  Op2 o a c  == Op2 p b d  = (o,a,c) == (p,b,d)
  (f :$ a)   == (g :$ b)   = (Just f,Just a)==(cast g,cast b)
  _ == _ = False

instance Num a => Num (b->a) where
  a+b = (\x -> a x + b x)
  a-b = (\x -> a x - b x)
  a*b = (\x -> a x * b x)
  fromInteger = const . fromInteger
  abs = (abs .)
  signum = (signum .)

instance (Typeable a, Num a) => Num (Expr a) where
  (+) = Op2 Add 
  (-) = Op2 Sub 
  (*) = Op2 Mul 
  fromInteger x = Static (show x) (fromInteger x)
  abs = Op1 Abs 
  signum = Op1 Signum

instance Fractional a => Fractional (b->a) where
  a/b = (\x -> a x / b x)
  fromRational = const . fromRational

instance (Typeable a, Fractional a) => Fractional (Expr a) where
  (/) = Op2 Div
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
    go (f :$ a) = go f :$ go a
    go (Op1 o a) = Op1 o (go a)
    go (Op2 o a b) = Op2 o (go a) (go b)
    go x        = (\i -> if i==i1 then i2 else i) %?  x



------------------------------------------------
-- Pretty printer for Expr
------------------------------------------------

ppr :: forall a. Expr a -> String
ppr x = case x of
  (Var x) -> x
  (Op1 o a) -> printf "%s(%s)" (show o) (ppr a)
  (Op2 Add a b) -> printf "%s+%s" (ppr a) (ppr b)
  (Op2 Sub a b) -> printf "%s-%s" (ppr a) (ppr b)
  (Op2 Mul a b) -> printf "%s %s" (ppr a) (ppr b)
  (Op2 Div a b) -> printf "\\frac{%s}{%s}" (ppr a) (ppr b)
  (Reserved Pair :$ a :$ b) -> printf "{%s,%s}" (ppr a) (ppr b)
  (Reserved Partial) -> "\\partial"
  (a :$ b) -> 
     let pprAp1 :: Expr Axis -> String
         pprAp1 b' = printf "%s_{%s}" (ppr a) (ppr b')
         pprAp2 :: Expr (Axis,Axis) -> String
         pprAp2 b' = printf "%s_{%s}" (ppr a) (ppr b')

         pprApDef :: Expr b -> String
         pprApDef b' = printf "%s\\!\\left(%s\\right)" (ppr a) (ppr b')
     in pprAp1 |||? pprAp2 |||? pprApDef  $ b
  (Static s _) -> s
  _ -> "?"

instance Show (Expr a) where show = ppr






stage :: Expr a -> Expr a
stage x@(f :$ a) = let f' = stage f
                       a' = stage a
                   in case (f', a') of
                        (Static sf f0, Static sa a0) -> (Static (ppr x) (f0 a0))
stage x = x


-- Apply the function at everywhere in an expresion
everywhere :: (Typeable a, Typeable b) => (Expr b->Expr b)->Expr a -> Expr a
everywhere f x = case x of
  (Op1 o a)   -> f %? (Op1 o (everywhere f a))
  (Op2 o a b) -> f %? (Op2 o (everywhere f a) (everywhere f b))
  (g :$ a)    -> f %? ((everywhere f g) :$ (everywhere f a))
  _           -> f %? x

-- Apply the function at everywhere in a statement
everywhereS :: (Typeable a, Typeable b) => (Expr b->Expr b)-> Stmt a -> Stmt a
everywhereS f (lhs := rhs) = (everywhere f lhs := everywhere f rhs)



