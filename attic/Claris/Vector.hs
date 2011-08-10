{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Dynamic
import           Language.Paraiso.Generator (generate)
import qualified Language.Paraiso.Generator.Claris as C
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

main :: IO ()
main = do
  _ <- generate (sampleProgram 4 8) "./" 
  return ()

sampleProgram :: Int -> Int -> C.Program
sampleProgram x1 x2 = 
  C.Program {
    C.language = Native.CPlusPlus,
    C.progName = mkName "vector",
    C.topLevel = 
      [ include C.Chevron "iostream" ,
        include C.Chevron "vector" ,
        C.Exclusive C.SourceFile $ C.StmtExpr $ C.VarDeclSub varN (intImm 256),
        C.FuncDecl $ (C.function tInt (mkName "main"))
          { C.funcBody= mainBody }, 
        C.FuncDecl $ (C.function tInt (mkName "calc"))
          { C.funcArgs = [varX, varY] ,
            C.funcBody = calcBody
          }
      ]
    }
  where
    include p = C.Exclusive C.SourceFile . C.StmtPrpr . C.PrprInclude p
    
    varI = C.Var tInt (mkName "i") 
    varX = C.Var tInt (mkName "x") 
    varY = C.Var tInt (mkName "y")
    varZ = C.Var tInt (mkName "z")
    
    varN = C.Var (C.Const tInt) (mkName "N")
    
    varXs = C.Var tVecInt (mkName "xs") 
    
    mainBody = 
      [ C.StmtExpr $ C.VarDeclCon  varXs  (intImm 0),
        C.StmtFor 
          (C.VarDeclCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.Member (C.VarExpr varXs) (C.FuncCallStd "size" []) ))
          (C.Op1Prefix "++" (C.VarExpr varI))

          [ C.StmtExpr $ C.Op2Infix "=" (C.ArrayAccess (C.VarExpr varXs) (C.VarExpr varI)) (C.VarExpr varI)
          ] , 
        C.StmtExpr   $ cout << message << endl,
        C.StmtReturn $ intImm 0 ]

    calcBody = 
      [C.StmtExpr $ C.VarDeclSub varZ (intImm 10),
       C.StmtExpr $ C.Op2Infix "+=" (C.VarExpr varZ) 
       $ C.Op2Infix "*" (C.VarExpr varX) (C.VarExpr varY),
       C.StmtReturn $ (C.VarExpr varZ) 
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    message = C.FuncCallUsr (mkName "calc") [C.toDyn x1, C.toDyn x2]

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    intImm :: Int -> C.Expr
    intImm = C.toDyn

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)

    tVecInt :: C.TypeRep
    tVecInt = C.TemplateType "std::vector" [tInt]

    tV2Int :: C.TypeRep
    tV2Int = C.TemplateType "std::vector" [tVecInt]

