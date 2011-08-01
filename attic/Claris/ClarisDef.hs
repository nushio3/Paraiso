{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisDef (
  Program(..),
  
  Pragma(..), TopLevel (..), Function(..), Qualifier(..), Statement(..), 
  Var(..), Expr(..), Parenthesis(..)
  ) where

import Data.Dynamic
import Util

data Program 
  = Program {
    progName :: Text,
    topLevel :: [TopLevel] }
instance Nameable Program where name = progName

data TopLevel 
  = PragmaDecl Pragma
  | FuncDecl Function
  | UsingNamespaceStandard

data Pragma 
  = PragmaInclude {
    includeName :: Text,
    includeToHeader :: Bool,
    includeParen :: Parenthesis }
  | PragmaOnce
    
data Function 
  = Function {
    funcName :: Text, 
    funcQual :: [Qualifier],
    funcType :: TypeRep,
    funcArgs :: [Var],
    funcBody :: [Statement] }

data Qualifier
  = Member 
  | Global
  | Device
  | Host
  | Constant
  | Shared

data Statement 
  = StmtExpr Expr
  | StmtDecl Var Expr
  | StmtReturn Expr
  | StmtLoop 

data Var = Var TypeRep Text
instance Nameable Var where name (Var _ x) = x

data Expr
  = Imm Dynamic 
  | VarExpr Var
  | Op1Prefix Text Expr
  | Op2Infix Text Expr Expr
  | Op3Infix Text Text Expr Expr Expr
    
data Parenthesis 
  = Paren | Bracket | Brace 
  | Chevron | Chevron2 | Chevron3 
  | Quotation | Quotation2
