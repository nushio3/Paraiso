{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, RankNTypes, StandaloneDeriving, TupleSections #-}
import Data.List
import Text.Printf
import Data.Dynamic
import Linear

type VarName = String

data Axis = X | Y | Z
  deriving (Eq, Ord, Typeable)

type Pt = V3 Rational

data Symbol = Add | Sub | Mul | Div | Partial | Pair
  deriving (Eq, Ord, Show, Read)            

data Expr a where
  Var :: VarName -> Expr a
  Static :: String -> a -> Expr a
  Reserved :: Symbol -> Expr a
  (:$) :: (Typeable b) => Expr (b->a) -> Expr b -> Expr a
  deriving (Typeable)

--- alternative call with type-safe cast
(|||?) :: (Typeable a,Typeable c) => (c->b)->(a->b)->(a->b)
(|||?) g f x = case cast x of
  Just x' -> g x'
  _       -> f x

--- type-specific modification with type-safe cast
(%|||?) :: (Typeable a,Typeable b) => (b->b) -> (a->a) -> (a->a)
(%|||?) g f x = case cast x of
  Just x' -> case cast (g x') of
    Just y -> y
    _      -> f x
  _       -> f x


infixr 9 |||? 
infixr 9 %|||? 

infixl 1 :$

instance (Typeable a) => Eq (Expr a) where
  Var a      == Var b      = a==b
  Static a _ == Static b _ = a==b
  Reserved a == Reserved b = a==b
  (f :$ a)   == (g :$ b)   = (Just f,Just a)==(cast g,cast b)
  _ == _ = False

instance (Typeable a, Num a) => Num (Expr a) where
  a+b = Reserved Add :$ a :$ b
  a-b = Reserved Sub :$ a :$ b
  a*b = Reserved Mul :$ a :$ b  
  fromInteger x = Static (show x) (fromInteger x)
  abs = error "abs undefined for Expr"
  signum = error "signum undefined for Expr"

instance Num a => Num (b->a) where
  a+b = (\x -> a x + b x)
  a-b = (\x -> a x - b x)
  a*b = (\x -> a x * b x)
  fromInteger = const . fromInteger
  abs = error "abs undefined for Func"
  signum = error "signum undefined for Func"


instance (Typeable a, Fractional a) => Fractional (Expr a) where
  a/b= Reserved Div :$ a :$ b
  fromRational x = Static (show x) (fromRational x)

instance Fractional a => Fractional (b->a) where
  a/b = (\x -> a x / b x)
  fromRational = const . fromRational

ppr :: forall a. Expr a -> String
ppr x = case x of
  (Var x) -> x
  (Reserved Add :$ a :$ b) -> printf "%s+%s" (ppr a) (ppr b)
  (Reserved Sub :$ a :$ b) -> printf "%s-%s" (ppr a) (ppr b)
  (Reserved Mul :$ a :$ b) -> printf "%s %s" (ppr a) (ppr b)
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

-- extract all axis variable names from Expr
axisVarsIn :: (Typeable a) => Expr a -> [VarName]
axisVarsIn x = nub $ sort $ go x
  where
    go :: (Typeable a) => Expr a -> [VarName]
    go x@(Var n) = let fromA :: Expr Axis -> [VarName]
                       fromA = const [n]
                   in fromA |||? const [] $ x
    go (Static _ _) = []
    go (Reserved _) = []
    go (f :$ a)     = go f ++ go a

replaceTerm :: (Typeable a, Typeable b) => Expr b -> Expr b -> Expr a -> Expr a
replaceTerm i1 i2 = go
  where
    go :: Typeable a => Expr a -> Expr a
    go (f :$ a) = go f :$ go a
    go x        = (\i -> if i==i1 then i2 else i) %|||? id  $ x


delta' :: (Num a) => Expr ((Axis,Axis) -> a)
delta' = Static "\\delta" (\(i,j)-> if i==j then 1 else 0)

delta :: (Num a, Typeable a) => (Expr Axis, Expr Axis) -> Expr a
delta (i,j) = delta' :$ (Reserved Pair :$ i :$ j)


mkT1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr a
mkT1 n i = (Var n :: Expr (Axis->a)) :$ i

mkT2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr a
mkT2 n (i,j) = (Var n :: Expr ((Axis,Axis)->a)) :$ (Reserved Pair :$ i :$ j)

mkTF1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr Pt -> Expr a
mkTF1 n i r = (Var n :: Expr (Axis->Pt->a)) :$ i :$ r

mkTF2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr Pt -> Expr a
mkTF2 n (i,j) r = (Var n :: Expr ((Axis,Axis)->Pt->a)) :$ (Reserved Pair :$ i :$ j) :$ r


main :: IO ()
main = do
  let i = Var "i" :: Expr Axis
      j = Var "j" :: Expr Axis

      r = Var "r" :: Expr Pt

      sigma = mkTF2 "\\sigma" 
      f = mkTF1 "f" 

  print $ (delta (i,j)        :: Expr Double)
  print $ (f(i) r             :: Expr Double)
  print $ (sigma (i,j) r      :: Expr Double)
  print $ (sigma (i,j) + f(i) $ r:: Expr Double)
  print $ axisVarsIn (sigma (i,j) + f(i) $ r:: Expr Double)