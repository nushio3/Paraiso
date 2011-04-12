{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
  VVMCompiler compiles the VVM program to native (c++) programs.
-}

module VVMCompiler (
                    NativeMachine(..), X86(..),
                    ProblemConfiguration(..)
                   ) where

import Data.List
import Data.Either

import VVM


{- 
   a NativeMachine, given a ProblemConfiguration and VVM configuration,
   compiles a VVM program to native program.
 -}
class NativeMachine a where
  compile:: a -> ProblemConfiguration -> VVM -> DAG -> String
  
{-
  duration : how many generations you want to simulate
  extent : the size of cellular automata world
  initialConfiguration : list of initially on cells
 -}
data ProblemConfiguration =  ProblemConfiguration 
  {
    duration::Int,
    extent::Coordinate,
    initialConfiguration::[(Coordinate)]
  }




--    generate c++ names
data Context = InLoop | OutOfLoop

class Named a where
  name :: a -> String
  nameWithin :: Context -> a -> String 
  nameWithin = undefined
  
instance Named Register where
  name r = case r of
             (StaticRhs i) -> "s" ++ show i
             (StaticLhs i) -> "new_s" ++ show i
             (Local i) -> "a" ++ show i
             
instance Named Coordinate where
  name (x, y) = "[((y+" ++ show y ++ "+kSizeY)%kSizeY)*kSizeX + " ++
                "(x+" ++ show x ++ "+kSizeX)%kSizeX]" 
  nameWithin OutOfLoop (x, y) =  "[((" ++ show y ++ "+kSizeY)%kSizeY)*kSizeX + " ++
                                 "(" ++ show x ++ "+kSizeX)%kSizeX]" 
                                                                     
instance Named ValueType where
  name = show
  

-- an implementation of VVM compiler.
  
data X86 = X86
instance NativeMachine X86 where
  compile = x86compile

x86compile :: X86 -> ProblemConfiguration -> VVM -> DAG -> String
x86compile x86 prob vm dag = header ++ kernel ++ footer
    where
      (sx, sy) = extent prob
      header = "#include <algorithm>\n" ++ 
               "#include <iostream>\n" ++ 
               "#include <vector>\n" ++ 
               "#include <unistd.h>\n" ++ 
               "using namespace std;\n" ++ 
               "const int kSizeX = " ++ show sx ++ ";\n" ++
               "const int kSizeY = " ++ show sy ++ ";\n" ++
               staticDefinitions ++ "\n" ++
               "int main (){\n" ++
               initialization ++
               "for (int t = 0; t < " ++ show (duration prob)++  "; ++t) {\n" ++
               "for (int y = 0; y < kSizeY; ++y) {\n" ++ 
               "for (int x = 0; x < kSizeX; ++x) {\n" 
      footer = "\n}\n}\n" ++ 
               staticSwaps ++ "\n" ++
               output ++ 
               "}\n" ++ 
               "}\n"
      output = "for (int y = 0; y < kSizeY; ++y) {\n" ++ 
               "for (int x = 0; x < kSizeX; ++x) {\n" ++
               "cout << " ++ name (StaticRhs 0) ++ name origin ++ ";" ++
               "}\ncout << endl;\n};\n" ++
               "cout << endl;usleep(100000);\n"
      origin::Coordinate
      origin = (0,0)
               
      staticDefinitions = intercalate "\n" $ map staticDef $ concat $
                          [[StaticLhs i, StaticRhs i]|i<-[0..staticSize vm-1]]
      staticDef r = "vector<int> " ++ name r ++ "(kSizeX*kSizeY);"
      
      staticSwaps = intercalate "\n" $ map staticSwap $ 
                    [(StaticLhs i, StaticRhs i)|i<-[0..staticSize vm-1]]
      staticSwap (l,r) = "swap(" ++ name l ++ "," ++ name r ++ ");"
      
      initialization = concat 
        [name (StaticRhs 0) ++ nameWithin OutOfLoop (x,y) ++ "=1;\n" |(x,y) <- initialConfiguration prob]
      
      kernel = intercalate "\n" $ map translate (nodes dag)
      translate (lhs, rhs) = let
        lhsstr = case lhs of
          StaticLhs i -> name lhs ++ name lhscoord
          Local  i -> "const int " ++ name lhs
        lhscoord = case rhs of
          Store co _ -> co
        rhsstr = case rhs of
          Load a co -> name a ++ name co
          Store _ a -> name a
          Add a b   -> binop "+" a b 
          Sub a b   -> binop "-" a b
          Imm val   -> name val
          And a b   -> binop "&&" a b
          Or  a b   -> binop "||" a b
          IsEqual a b -> binop "==" a b
          Select a b c -> name a ++ " ? " ++ name b ++ " : " ++ name c
        binop opstr a b = name a ++ " " ++ opstr ++ " " ++ name b
        in lhsstr ++ " = (" ++ rhsstr ++ ");"
