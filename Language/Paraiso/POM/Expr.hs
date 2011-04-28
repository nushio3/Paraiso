{-# OPTIONS -Wall #-}
module Language.Paraiso.POM.Expr (Expr(..)) where


data Expr op a = Term a | Expr op [Expr op a]