{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GADTs, ScopedTypeVariables, RankNTypes, StandaloneDeriving, TupleSections #-}
module Tensor where

import Control.Applicative
import Data.Dynamic
import Data.List
import Data.Ratio
import Text.Printf

import Expr
import Transformation

-- extract all axis variables from Expr
axisVarsIn :: (Typeable a) => Expr a -> [Expr Axis]
axisVarsIn x = nub $ go x
  where
    go :: (Typeable a) => Expr a -> [Expr Axis]
    go (Static _ _) = []
    go (Reserved _) = []
    go (Op1 _ _ a)  = go a
    go (Op2 _ _ a b)= go a ++ go b
    go (f :$ a)     = go f ++ go a
    go x = let fromA :: Expr Axis -> [Expr Axis]
               fromA = (:[])
           in fromA |||? const [] $ x

einsteinRule :: (Typeable a, Num a) => Stmt a -> [Stmt a]
einsteinRule (lhs := rhs) = ret
  where
    lhsi = axisVarsIn lhs
    rhsi = axisVarsIn rhs

    freei = [i | i <- rhsi , not (i `elem` lhsi)]
    boundi = lhsi

    rhs1 = foldr ($) rhs [byTerms (sumOver j)| j <- freei] 
    
    ret =  foldr (=<<) [lhs := rhs1] [specializeStmt i| i <- boundi] 


sumOver :: (Typeable a, Num a) => Expr Axis -> Expr a -> Expr a
sumOver i  expr 
 | i `elem` axisVarsIn expr = foldr1 (+) [replaceAtom i j expr | j <- axes]
 | otherwise = expr

specializeStmt :: Typeable a => Expr Axis -> Stmt a -> [Stmt a]
specializeStmt i (lhs := rhs) = 
  [let f = replaceAtom i j in f lhs := f rhs| j <- axes]

byTerms :: Typeable a => (Expr a -> Expr a) -> Expr a -> Expr a
byTerms f (Op2 Add g a b) = Op2 Add g (byTerms f a) (byTerms f b)
byTerms f (Op2 Sub g a b) = Op2 Sub g (byTerms f a) (byTerms f b)
byTerms f x = f x


mkT1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr a
mkT1 n i = (Var n :: Expr (Axis->a)) :$ i

mkT2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr a
mkT2 n (i,j) = (Var n :: Expr ((Axis,Axis)->a)) :$ (Reserved Pair :$ i :$ j)

mkTF0 :: forall a. (Typeable a) => VarName -> Expr (Pt -> a)
mkTF0 n = (Var n :: Expr (Pt->a))

mkTF1 :: forall a. (Typeable a) => VarName -> Expr Axis -> Expr (Pt -> a)
mkTF1 n i = (Var n :: Expr (Axis->Pt->a)) :$ i 

mkTF2 :: forall a. (Typeable a) => VarName -> (Expr Axis, Expr Axis) -> Expr (Pt -> a)
mkTF2 n (i,j) = (Var n :: Expr ((Axis,Axis)->Pt->a)) :$ (Reserved Pair :$ i :$ j) 

---- Combinators
delta :: (Num a, Typeable a) => (Expr Axis, Expr Axis) -> Expr a
delta (i,j) = delta' :$ (Reserved Pair :$ i :$ j)

δ :: (Num a, Typeable a) => (Expr Axis, Expr Axis) -> Expr a
δ = delta 

delta' :: (Num a) => Expr ((Axis,Axis) -> a)
delta' = Static "\\delta" (\(i,j)-> if i==j then 1 else 0)
