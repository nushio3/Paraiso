{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GADTs, ScopedTypeVariables, RankNTypes, StandaloneDeriving, TupleSections #-}
import Control.Applicative
import Data.Dynamic
import Data.List
import Data.Ratio
import Linear
import Text.Printf


type VarName = String

data Axis = X | Y | Z
  deriving (Eq, Ord, Typeable)

type Pt = Axis -> Rational

data Symbol = Partial | Pair
  deriving (Eq, Ord, Show, Read)            
data Op1Symbol = Abs | Signum
  deriving (Eq, Ord, Show, Read)            
data Op2Symbol = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show, Read)            

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

infix 1 :=
data Stmt a where
  (:=) :: Expr a -> Expr a -> Stmt a 

instance Show (Stmt a) where
  show (a := b) = show a ++ " &=& " ++ show b


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

infixl 2 :$

instance (Typeable a) => Eq (Expr a) where
  Var a      == Var b      = a==b
  Static a _ == Static b _ = a==b
  Reserved a == Reserved b = a==b
  Op1 o a    == Op1 p b    = (o,a) == (p,b)
  Op2 o a c  == Op2 p b d  = (o,a,c) == (p,b,d)
  (f :$ a)   == (g :$ b)   = (Just f,Just a)==(cast g,cast b)
  _ == _ = False

instance (Typeable a, Num a) => Num (Expr a) where
  (+) = Op2 Add 
  (-) = Op2 Sub 
  (*) = Op2 Mul 
  fromInteger x = Static (show x) (fromInteger x)
  abs = Op1 Abs 
  signum = Op1 Signum

instance Num a => Num (b->a) where
  a+b = (\x -> a x + b x)
  a-b = (\x -> a x - b x)
  a*b = (\x -> a x * b x)
  fromInteger = const . fromInteger
  abs = (abs .)
  signum = (signum .)


instance (Typeable a, Fractional a) => Fractional (Expr a) where
  (/) = Op2 Div
  fromRational x = Static (show x) (fromRational x)

instance Fractional a => Fractional (b->a) where
  a/b = (\x -> a x / b x)
  fromRational = const . fromRational

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
     let pprApply1 :: Expr Axis -> String
         pprApply1 b' = printf "%s_{%s}" (ppr a) (ppr b')
         pprApply2 :: Expr (Axis,Axis) -> String
         pprApply2 b' = printf "%s_{%s}" (ppr a) (ppr b')

         pprApplyDef :: Expr b -> String
         pprApplyDef b' = printf "%s(%s)" (ppr a) (ppr b')
     in pprApply1 |||? pprApply2 |||? pprApplyDef  $ b
  (Static s _) -> s
  _ -> "?"

instance Show (Expr a) where show = ppr

-- extract all axis variables from Expr
axisVarsIn :: (Typeable a) => Expr a -> [Expr Axis]
axisVarsIn x = nub $ go x
  where
    go :: (Typeable a) => Expr a -> [Expr Axis]
    go (Static _ _) = []
    go (Reserved _) = []
    go (f :$ a)     = go f ++ go a
    go x = let fromA :: Expr Axis -> [Expr Axis]
               fromA = (:[])
           in fromA |||? const [] $ x

replaceAtom :: (Typeable a, Typeable b) => Expr b -> Expr b -> Expr a -> Expr a
replaceAtom i1 i2 = go
  where
    go :: Typeable a => Expr a -> Expr a
    go (f :$ a) = go f :$ go a
    go x        = (\i -> if i==i1 then i2 else i) %?  x


einsteinRule :: (Typeable a, Num a) => Stmt a -> [Stmt a]
einsteinRule (lhs := rhs) = ret
  where
    lhsi = axisVarsIn lhs
    rhsi = axisVarsIn rhs

    freei = [i | i <- rhsi , not (i `elem` lhsi)]
    boundi = lhsi

    rhs1 = foldr ($) rhs [byTerms (sumOver j)| j <- freei] 
    
    ret =  foldr (=<<) [lhs := rhs1] [specializeStmt i| i <- boundi] 

axes :: [Expr Axis]
axes = [Static "x" X, Static "y" Y, Static "z" Z]

sumOver :: (Typeable a, Num a) => Expr Axis -> Expr a -> Expr a
sumOver i  expr 
 | i `elem` axisVarsIn expr = foldr1 (+) [replaceAtom i j expr | j <- axes]
 | otherwise = expr

specializeStmt :: Typeable a => Expr Axis -> Stmt a -> [Stmt a]
specializeStmt i (lhs := rhs) = 
  [let f = replaceAtom i j in f lhs := f rhs| j <- axes]

byTerms :: Typeable a => (Expr a -> Expr a) -> Expr a -> Expr a
byTerms f (Op2 Add a b) = Op2 Add (byTerms f a) (byTerms f b)
byTerms f (Op2 Sub a b) = Op2 Sub (byTerms f a) (byTerms f b)
byTerms f x = f x


---- Constructions

delta' :: (Num a) => Expr ((Axis,Axis) -> a)
delta' = Static "\\delta" (\(i,j)-> if i==j then 1 else 0)

delta :: (Num a, Typeable a) => (Expr Axis, Expr Axis) -> Expr a
delta (i,j) = delta' :$ (Reserved Pair :$ i :$ j)


mkT1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr a
mkT1 n i = (Var n :: Expr (Axis->a)) :$ i

mkT2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr a
mkT2 n (i,j) = (Var n :: Expr ((Axis,Axis)->a)) :$ (Reserved Pair :$ i :$ j)

mkTF1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr (Pt -> a)
mkTF1 n i = (Var n :: Expr (Axis->Pt->a)) :$ i 

mkTF2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr (Pt -> a)
mkTF2 n (i,j) = (Var n :: Expr ((Axis,Axis)->Pt->a)) :$ (Reserved Pair :$ i :$ j) 

partial :: forall a. (Typeable a) => Expr Axis -> Expr (Pt -> a) -> Expr (Pt -> a)
partial i f = (Reserved Partial :: Expr (Axis -> (Pt->a)->(Pt->a))) :$ i :$ f

partial4 :: forall a. (Typeable a, Fractional a) => Expr Axis -> Expr (Pt -> a) -> Expr (Pt -> a)
partial4 i f = Static "\\partial" partial4' :$ i :$ f 

partial4' :: (Fractional a) => Axis -> (Pt->a) -> (Pt->a)
partial4' i f p = (f (p + 0.5 *^ e(i)) - f (p - 0.5 *^ e(i))) * (fromRational $ 9%8)
              - (f (p + 1.5 *^ e(i)) - f (p - 1.5 *^ e(i))) * (fromRational $ 1%24)
  where
    e :: Axis -> Pt
    e X = V3 1 0 0
    e Y = V3 0 1 0
    e Z = V3 0 0 1

partial4'' :: (Fractional a) => Axis -> (Pt->a) -> (Pt->a)
partial4'' i f p = (f (p + 0.5 *^ e(i)) - f (p - 0.5 *^ e(i))) * (fromRational $ 9%8)
              - (f (p + 1.5 *^ e(i)) - f (p - 1.5 *^ e(i))) * (fromRational $ 1%24)
  where
    e :: Axis -> Pt
    e X = V3 1 0 0
    e Y = V3 0 1 0
    e Z = V3 0 0 1



main :: IO ()
main = do
  let i = Var "i" :: Expr Axis
      j = Var "j" :: Expr Axis

      r = Var "r" :: Expr Pt

      sigma = mkTF2 "\\sigma" 
      f = mkTF1 "f" 
      
      eqV :: Stmt (Pt -> Double)
      eqV = f(i)  := (partial(j)(sigma(i,j)) + f(i)) 

      eqV' :: Stmt Double
      eqV' = f(i) :$ r := ((partial(j)(sigma(i,j)) :$ r) + (f(i) :$ r)) 
--      eqV = f(i) r := f i r

  print $ (delta (i,j)        :: Expr Double)
  print $ (f(i) :$ r             :: Expr Double)
  print $ (sigma (i,j) :$ r      :: Expr Double)
  print $ (sigma (i,j) + f(i) :$ r:: Expr Double)
  mapM_ print $ einsteinRule $ eqV
  mapM_ print $ einsteinRule $ eqV'
