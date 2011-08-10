{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Generator.ClarisDef (      
  Program(..),

  FileType(..),
  Preprocessing(..), TopLevelElem (..), 
  TypeRep(..), typeOf, toDyn,
  Function(..), function, 
  Qualifier(..), Statement(..), 
  Var(..), 
  Expr(..), Parenthesis(..)
  ) where

import qualified Data.Dynamic as Dyn
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude


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

data TypeRep 
  = UnitType     Dyn.TypeRep
  | PtrOf        TypeRep
  | RefOf        TypeRep
  | Const        TypeRep
  | TemplateType Text [TypeRep]
  | UnknownType
  deriving (Eq, Show)    

typeOf :: (Dyn.Typeable a) => a -> TypeRep
typeOf = UnitType . Dyn.typeOf

data Qualifier
  = CudaGlobal
  | CudaDevice
  | CudaHost
  | CudaShared
  | CudaConst
  deriving (Eq, Show)                        

data Statement 
  = StmtExpr Expr
  | StmtDecl Var 
  | StmtDeclCon Var Expr
  | StmtDeclSub Var Expr
  | StmtReturn Expr
  | StmtWhile Expr [Statement]
  | StmtFor Statement Expr Expr [Statement]
  deriving (Eq, Show)                    

data Var = Var TypeRep Name deriving (Eq, Show)
instance Nameable Var where name (Var _ x) = x


data Expr
  = Imm Dyn.Dynamic 
  | VarExpr Var
  | FuncCallUsr Name [Expr]
  | FuncCallStd Text [Expr]
  | Member Expr Expr
  | Op1Prefix Text Expr
  | Op1Postfix Text Expr
  | Op2Infix Text Expr Expr
  | Op3Infix Text Text Expr Expr Expr
  | ArrayAccess Expr Expr
  deriving (Show)

toDyn :: (Dyn.Typeable a) => a -> Expr
toDyn = Imm . Dyn.toDyn

instance Eq Expr where
  (==)_ _= error "cannot compare Expr."
instance Ord Expr where
  compare _ _ = error "cannot compare Expr."

data Parenthesis 
  = Paren | Bracket | Brace 
  | Chevron | Chevron2 | Chevron3 
  | Quotation | Quotation2
  deriving (Eq, Show)                    
