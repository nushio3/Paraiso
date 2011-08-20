{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the higher-level concepts to native languages with
--  capability to describe C++ syntax such as classes and
--  containers. Claris also have support for extension made by
--  C++-like languages such as CUDA qualifier and kernel call.
--
-- The design goal of Claris is to cover the necessity of the code
-- generation and to make it simple. Claris is not designed for
-- syntatic correctness, and it's possible to describe a Claris code
-- that will cause a compile error in C++.
-- 
-- In Claris, variables, functions and classes are described in a
-- unified manner that supports both the declaration and definition.
-- From that information, the declarations and definitions are
-- generated at appropriate places.

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

-- | A Claris program.
data Program 
  = Program 
    { progName :: Name, -- ^ the name of the program
      topLevel :: [Statement]  -- ^ the top-level elements of the program.
    } 
  deriving (Show)
instance Nameable Program where name = progName

-- | C++ class descriptions are separated to two files
data FileType 
  = HeaderFile
  | SourceFile 
  deriving (Eq, Show)

-- | C++ top-level statements 
data Statement 
  = StmtPrpr Preprocessing       -- ^ Preprosessor directive
  | UsingNamespace Name          -- ^ Name space declaration
  | ClassDef Class               -- ^ Class definition
  | FuncDef Function             -- ^ Function definition
  | VarDef Var                   -- ^ variable definition as an expression 
  | VarDefCon Var [Expr]         -- ^ define a variable and call a constructor
  | VarDefSub Var Expr           -- ^ define a variable and substitute a value
  | StmtExpr Expr                -- ^ Expression
  | StmtWhile Expr [Statement]   -- ^ While loop
  | StmtFor Statement Expr Expr 
    [Statement]                  -- ^ For loop
  | StmtReturn Expr              -- ^ return 
  | Exclusive FileType Statement -- ^ A statement that is included exclusively 
                                 --   in either of the file type
  
  | RawStatement Text            -- ^ text directly embedded into source code
  | Comment Text                 -- ^ a comment
  deriving (Eq, Show)                    

-- | Preprocessor directive 
data Preprocessing
  = PrprInclude Parenthesis Text
  | PrprPragma Text
  deriving (Eq, Show)    

-- | C++ class
data Class 
  = Class 
    { className :: Name,
      classMember :: [MemberDef]
    }
  deriving (Eq, Show)
instance Nameable Class where name = className

-- | C++ class member definition
data MemberDef 
  = MemberFunc
    { memberAccess :: AccessModifier,
      inlined      :: Bool,
      memberFunc   :: Function
    }  -- ^ A member function
  | MemberVar 
    { memberAccess :: AccessModifier, 
      memberVar :: Var
    }-- ^ A member variable
  deriving (Eq, Show)

-- | C++ class member access modifier
data AccessModifier = Private | Protected | Public
  deriving (Eq, Show)

-- | C++ syntax for variable definition


-- | C++ function definition
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

-- | description C++ type
data TypeRep 
  = UnitType     Dyn.TypeRep -- ^ Types for simple objects
  | PtrOf        TypeRep     -- ^ Pointer type
  | RefOf        TypeRep     -- ^ Reference type
  | Const        TypeRep     -- ^ Constant type
  | TemplateType Text [TypeRep] -- ^ A template type
  | QualifiedType [Qualifier] TypeRep -- ^ Qualified type
  | ConstructorType                    
    -- ^ the type of mu which is returned from constructor / destructor  
  | UnknownType
    -- ^ the type of kuu that is detached from reincarnation
  deriving (Eq, Show)    

-- | [CUDA extension] qualifiers to use accelerator
data Qualifier
  = CudaGlobal
  | CudaDevice
  | CudaHost
  | CudaShared
  | CudaConst
  deriving (Eq, Show)                        

-- | C++ Variable definition
data Var = Var TypeRep Name deriving (Eq, Show)
instance Nameable Var where name (Var _ x) = x

-- | C++ Expression
data Expr
  = Imm Dyn.Dynamic -- ^ an immediate
  | VarExpr Var -- ^ an expression made of a variable
  | FuncCallUsr Name [Expr] -- ^ user function call
  | FuncCallStd Text [Expr] -- ^ builtin function call 
  | CudaFuncCallUsr Name Expr Expr [Expr] -- ^ cuda function call with Grid topology
  | MemberAccess Expr Expr -- ^ access a member of an object
  | Op1Prefix Text Expr -- ^ prefix unary operator
  | Op1Postfix Text Expr -- ^ postfix unary operator
  | Op2Infix Text Expr Expr -- ^ infix binary operator
  | Op3Infix Text Text Expr Expr Expr -- ^ sandwiched trinity operator
  | ArrayAccess Expr Expr -- ^ access a component of an array
  | CommentExpr Text Expr -- ^ commented expr
  deriving (Show)

instance Eq Expr where
  (==)_ _= error "cannot compare Expr."

-- | make C++ type from Haskell objects
typeOf :: (Dyn.Typeable a) => a -> TypeRep
typeOf = UnitType . Dyn.typeOf

-- | make a C++ expression from Haskell objects
toDyn :: (Dyn.Typeable a) => a -> Expr
toDyn = Imm . Dyn.toDyn

-- | parentheses used in C++
data Parenthesis 
  = Paren   -- ^ expression coupling, function call
  | Bracket -- ^ array access 
  | Brace   -- ^ create a code block
  | Chevron -- ^ tepmplate type
  | Chevron2 -- ^ not used
  | Chevron3 -- ^ CUDA kernel call
  | Quotation  -- ^ character
  | Quotation2 -- ^ string
  | SlashStar  -- ^ comment
  deriving (Eq, Show)                    
