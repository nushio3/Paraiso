{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}
import Text.Printf
import Data.Dynamic

type VarName = String

data Axis = X | Y | Z
  deriving (Eq, Ord)

data Symbol = Add | Sub | Mul | Div | Partial
  deriving (Eq, Ord, Show, Read)            

data Expr a where
  Var :: VarName -> Expr a
  Static :: String -> a -> Expr a
  Reserved :: Symbol -> Expr a
  (:$) :: (Typeable b) => Expr (b->a) -> Expr b -> Expr a
  deriving (Typeable)


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
  
instance (Typeable a, Fractional a) => Fractional (Expr a) where
  a/b= Reserved Div :$ a :$ b
  fromRational x = Static (show x) (fromRational x)

ppr :: Expr a -> String
ppr x = case x of
  (Var x) -> x
  (Reserved Add :$ a :$ b) -> printf "%s+%s" (ppr a) (ppr b)
  (Reserved Sub :$ a :$ b) -> printf "%s-%s" (ppr a) (ppr b)
  (Reserved Mul :$ a :$ b) -> printf "%s %s" (ppr a) (ppr b)
  (Reserved Partial :$ a :$ b) -> printf "\\partial_{%s} %s" (ppr a) (ppr b)
  (a :$ b) -> printf "%s(%s)" (ppr a) (ppr b)
  (Static s _) -> s
  _ -> "?"

x = (1+2 :: Expr Int)

main :: IO ()
main = do
  putStrLn $ ppr x
  print $ 1 == (1 :: Expr Int)
  print $ 1+1 == (2 :: Expr Int)
