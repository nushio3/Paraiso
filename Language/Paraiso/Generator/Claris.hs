{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the Orthotope Machine program to native languages
--  with capability to describe classes and containers, such as
--  C++. Claris may include


module Language.Paraiso.Generator.Claris (      
  Program(..),

  FileType(..),
  Statement(..), 
  Preprocessing(..), 
  TypeRep(..), typeOf, toDyn,
  Class(..), MemberDef(..), AccessModifier(..),
  Function(..), function, 
  Qualifier(..), 
  Var(..), 
  Expr(..), Parenthesis(..)
  ) where


import qualified Data.Dynamic as Dyn
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

data Program 
  = Program 
    { progName :: Name,
      topLevel :: [Statement] 
    } 
  deriving (Show)
instance Nameable Program where name = progName

data FileType 
  = HeaderFile
  | SourceFile 
  deriving (Eq, Show)

data Statement 
  = StmtPrpr Preprocessing 
  | UsingNamespace Name    
  | ClassDef Class
  | FuncDef Function
  | StmtExpr Expr
  | StmtWhile Expr [Statement]
  | StmtFor Expr Expr Expr [Statement]
  | StmtReturn Expr
  | Exclusive FileType Statement
  deriving (Eq, Show)                    

data Preprocessing
  = PrprInclude Parenthesis Text
  | PrprPragma Text
  deriving (Eq, Show)    

data Class 
  = Class 
    { className :: Name,
      classMember :: [MemberDef]
    }
  deriving (Eq, Show)
instance Nameable Class where name = className

data MemberDef 
  = MemberFunc
    { memberAccess :: AccessModifier,
      memberFunc :: Function
    }
  | MemberVar
    { memberAccess :: AccessModifier, 
      memberVar :: Var
    }
  deriving (Eq, Show)

data AccessModifier = Private | Protected | Public
  deriving (Eq, Show)

data Function 
  = Function 
    { funcName :: Name, 
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
    funcArgs = [],
    funcBody = []
  }

data TypeRep 
  = UnitType     Dyn.TypeRep
  | PtrOf        TypeRep
  | RefOf        TypeRep
  | Const        TypeRep
  | TemplateType Text [TypeRep]
  | QualifiedType [Qualifier] TypeRep
  | ConstructorType                    -- ^the type returned from constructor / destructor  
  | UnknownType
  deriving (Eq, Show)    

typeOf :: (Dyn.Typeable a) => a -> TypeRep
typeOf = UnitType . Dyn.typeOf


-- | [CUDA extension] 
data Qualifier
  = CudaGlobal
  | CudaDevice
  | CudaHost
  | CudaShared
  | CudaConst
  deriving (Eq, Show)                        

data Var = Var TypeRep Name deriving (Eq, Show)
instance Nameable Var where name (Var _ x) = x


data Expr
  = Imm Dyn.Dynamic 
  | VarExpr Var
  | VarDef Var 
  | VarDefCon Var Expr
  | VarDefSub Var Expr
  | FuncCallUsr Name [Expr]
  | FuncCallStd Text [Expr]
  | CudaFuncCallUsr Name Expr Expr [Expr]
  | MemberAccess Expr Expr
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
