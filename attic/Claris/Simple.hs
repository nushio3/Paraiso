{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Claris as C
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

main :: IO ()
main = do
  _ <- generateIO Native.defaultSetup{Native.language = Native.CPlusPlus} $ 
       sampleProgram 8 5
  return ()

sampleProgram :: Int -> Int -> C.Program
sampleProgram x1 x2 = 
  C.Program {
    C.progName = mkName "simple",
    C.topLevel = 
      [ C.Exclusive C.SourceFile $ C.StmtPrpr $ C.PrprInclude C.Chevron "iostream" ,
        C.FuncDef $ (C.function tInt (mkName "main"))
          { C.funcBody= mainBody }, 
        C.FuncDef $ (C.function tInt (mkName "calc"))
          { C.funcArgs = [varX, varY] ,
            C.funcBody = calcBody
          }
      ]
    }
  where
    varX = C.Var tInt (mkName "x") 
    varY = C.Var tInt (mkName "y")
    varZ = C.Var tInt (mkName "z")
    mainBody = 
      [C.StmtExpr   $ cout << message << endl,
       C.StmtReturn $ C.toDyn (0::Int) ]
    
    calcBody = 
      [C.StmtExpr $ C.VarDefSub varZ (C.toDyn(2::Int)),
       C.StmtExpr $ C.Op2Infix "+=" (C.VarExpr varZ) 
       $ C.Op2Infix "*" (C.VarExpr varX) (C.VarExpr varY),
       C.StmtReturn $ (C.VarExpr varZ) 
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    message = C.FuncCallUsr (mkName "calc") [C.toDyn x1, C.toDyn x2]

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)
