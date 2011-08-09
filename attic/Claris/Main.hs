{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Dynamic
import           Language.Paraiso.Generator (generate)
import qualified Language.Paraiso.Generator.Claris as C
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

main :: IO ()
main = do
  _ <- generate (sampleProgram 5 8) "./" 
  return ()

sampleProgram :: Int -> Int -> C.Program
sampleProgram x1 x2 = 
  C.Program {
    C.progName = mkName "main",
    C.topLevel = 
      [ C.PrprInst $ C.Include C.SourceFile C.Chevron "iostream" ,
        C.FuncDecl $ (C.function tInt (mkName "main"))
          { C.funcBody= mainBody }, 
        C.FuncDecl $ (C.function tInt (mkName "calc"))
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
      [C.StmtDeclInit varZ (C.Imm $ toDyn(2::Int)),
       C.StmtExpr $ C.Op2Infix "+=" (C.VarExpr varZ) 
       $ C.Op2Infix "*" (C.VarExpr varX) (C.VarExpr varY),
       C.StmtReturn $ (C.VarExpr varZ) 
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    message = C.FuncCallUser (mkName "calc") [C.toDyn x1, C.toDyn x2]

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)
