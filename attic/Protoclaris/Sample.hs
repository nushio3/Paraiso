{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Sample (
  helloWorld
  ) where

import Claris as C
import Data.Dynamic
import Util

tInt :: TypeRep
tInt = typeOf (undefined :: Int)


helloWorld :: C.Program
helloWorld = 
  C.Program {
    progName = "hello",
    topLevel = 
      [PragmaDecl $ PragmaInclude "iostream" False Chevron,
       FuncDef $ Function "main" [] tInt [] body]
    }
  where
    body = 
      [StmtExpr coutExpr,
       StmtReturn $ Imm $ toDyn (0::Int) ]
    cout = VarExpr $ Var unknownType "std::cout"
    endl = VarExpr $ Var unknownType "std::endl"
    message = Imm $ toDyn ("Hello, world!"::Text)
              
    infixl 1 <<
    (<<) = Op2Infix "<<"
    coutExpr = cout << message << endl
