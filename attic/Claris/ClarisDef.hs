{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisDef (
  Program(..)
  ) where

import Util


data Program = Program {
  progName  :: Text,
  functions :: [Function]
  }

instance Nameable Program where name = progName

data Function = Function {
  funcName :: Text, 
  funcType :: FuncType,
  funcBody :: [Statement]
  }
  
data FuncType = MemberFunc | KernelFunc                

data Statement = Statement
