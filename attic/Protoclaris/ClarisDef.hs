{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
OverloadedStrings, RankNTypes #-} 

{-# OPTIONS -Wall #-}
module ClarisDef (
  Program(..),

  Pragma(..), TopLevelElem (..), Function(..), Qualifier(..), Statement(..), 
  Var(..), UnknownType(..), unknownType,
  Expr(..), Parenthesis(..)
  ) where

import Data.Dynamic
import Util

data Program 
  = Program {
    progName :: Text,
    topLevel :: [TopLevelElem] }
instance Nameable Program where name = progName

data TopLevelElem 
  = PragmaDecl Pragma
  | FuncDef Function
  | UsingNamespace Text

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
  | StmtDecl Var 
  | StmtDeclInit Var Expr
  | StmtReturn Expr
  | StmtLoop 

data Var = Var TypeRep Text
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
