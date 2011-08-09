{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Generator.ClarisDef (      
  Program(..),

  FileType(..),
  Preprocessing(..), TopLevelElem (..), Function(..), function, 
  Qualifier(..), Statement(..), 
  Var(..), UnknownType(..), unknownType,
  Expr(..), Parenthesis(..)
  ) where

import Data.Dynamic
import Language.Paraiso.Name
import Language.Paraiso.Prelude


data Program 
  = Program 
    { progName :: Name,
      topLevel :: [TopLevelElem] 
    } 
  deriving (Show)
instance Nameable Program where name = progName

data FileType 
  = HeaderFile
  | SourceFile 
  deriving (Eq, Show)
           
data TopLevelElem 
  = PrprInst Preprocessing
  | FuncDecl Function
  | UsingNamespace Name 
  deriving (Eq, Show)

data Preprocessing
  = Include 
    { prprFileType :: FileType   ,
      includeParen :: Parenthesis,    
      includeFileName :: Text    
    }
  | Pragma 
    { prprFileType :: FileType,
      pragmaText :: Text      
    }
  deriving (Eq, Show)    
           
data Function 
  = Function 
    { funcName :: Name, 
      funcQual :: [Qualifier],
      funcType :: TypeRep,
      funcArgs :: [Var],
      funcBody :: [Statement] 
    }
  deriving (Eq, Show)
instance Nameable Function where name = funcName

-- | A default function maker
function :: TypeRep -> Name ->  Function
function tr na = Function
  { funcName = na,
    funcType = tr,
    funcQual = [],
    funcArgs = [],
    funcBody = []
  }

data Qualifier
  = Member 
  | Global
  | Device
  | Host
  | Constant
  | Shared
  deriving (Eq, Show)                        

data Statement 
  = StmtExpr Expr
  | StmtDecl Var 
  | StmtDeclInit Var Expr
  | StmtReturn Expr
  | StmtLoop 
  deriving (Eq, Show)                    

data Var = Var TypeRep Name deriving (Eq, Show)
instance Nameable Var where name (Var _ x) = x

data UnknownType = UnknownType deriving (Eq, Show, Typeable)
unknownType :: TypeRep
unknownType = typeOf UnknownType

data Expr
  = Imm Dynamic 
  | VarExpr Var
  | FuncCallUser    Name [Expr]
  | FuncCallBuiltin Text [Expr]
  | Op1Prefix Text Expr
  | Op1Postfix Text Expr
  | Op2Infix Text Expr Expr
  | Op3Infix Text Text Expr Expr Expr
  deriving (Show)

instance Eq Expr where
  (==)_ _= error "cannot compare Expr."
instance Ord Expr where
  compare _ _ = error "cannot compare Expr."

data Parenthesis 
  = Paren | Bracket | Brace 
  | Chevron | Chevron2 | Chevron3 
  | Quotation | Quotation2
  deriving (Eq, Show)                    
