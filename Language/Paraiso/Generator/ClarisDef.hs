{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Generator.ClarisDef (      
  Program(..),

  Pragma(..), TopLevelElem (..), Function(..), Qualifier(..), Statement(..), 
  Var(..), UnknownType(..), unknownType,
  Expr(..), Parenthesis(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Name
import Language.Paraiso.Prelude


data Program 
  = Program {
    progName :: Name,
    topLevel :: [TopLevelElem] }
instance Nameable Program where name = progName

data TopLevelElem 
  = PragmaDecl Pragma
  | FuncDecl Function
  | UsingNamespace Name

data Pragma 
  = PragmaInclude {
    includeName :: Name,
    includeToHeader :: Bool,
    includeParen :: Parenthesis }
  | PragmaOnce

data Function 
  = Function {
    funcName :: Name, 
    funcQual :: [Qualifier],
    funcType :: TypeRep,
    funcArgs :: [Var],
    funcBody :: [Statement] }
instance Nameable Function where name = funcName

data Qualifier
  = Member 
  | Global
  | Device
  | Host
  | Constant
  | Shared

data Statement 
  = StmtExpr Expr
  | StmtDecl Var 
  | StmtDeclInit Var Expr
  | StmtReturn Expr
  | StmtLoop 

data Var = Var TypeRep Name
instance Nameable Var where name (Var _ x) = x

data UnknownType = UnknownType deriving (Eq, Show, Typeable)
unknownType :: TypeRep
unknownType = typeOf UnknownType



data Expr
  = Imm Dynamic 
  | VarExpr Var
  | FuncCall Text [Expr]
  | Op1Prefix Text Expr
  | Op1Postfix Text Expr
  | Op2Infix Text Expr Expr
  | Op3Infix Text Text Expr Expr Expr

data Parenthesis 
  = Paren | Bracket | Brace 
  | Chevron | Chevron2 | Chevron3 
  | Quotation | Quotation2
