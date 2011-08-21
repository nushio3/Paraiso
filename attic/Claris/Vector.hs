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
       sampleProgram
  return ()

sampleProgram :: C.Program
sampleProgram = 
  C.Program {
    C.progName = mkName "vector",
    C.topLevel = 
      [ include C.Chevron "iostream" ,
        include C.Chevron "vector" ,
        C.Exclusive C.SourceFile $ C.StmtExpr $ C.VarDefSub varN (intImm 101),
        C.FuncDef $ (C.function tInt (mkName "main"))
          { C.funcBody= mainBody }, 
        C.FuncDef $ (C.function tVoid (mkName "calc"))
          { C.funcArgs = [varPX, varPY] ,
            C.funcBody = calcBody
          }
      ]
    }
  where
    include p = C.Exclusive C.SourceFile . C.StmtPrpr . C.PrprInclude p
    
    varI  = C.Var tInt (mkName "i") 
    varPX = C.Var (C.PtrOf tInt) (mkName "px") 
    varPY = C.Var (C.PtrOf tInt) (mkName "py")
    
    varN = C.Var (C.Const tInt) (mkName "N")
    
    varXs = C.Var tVecInt (mkName "xs") 
    varYs = C.Var tVecInt (mkName "ys") 
    
    rawPtr xs = C.Op1Prefix "&" $ C.ArrayAccess (C.VarExpr xs) (intImm 0)
    
    mainBody = 
      [ C.StmtExpr $ C.VarDefCon  varXs (C.VarExpr varN),
        C.StmtExpr $ C.VarDefCon  varYs (C.VarExpr varN),
        C.StmtFor 
          (C.VarDefCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.MemberAccess (C.VarExpr varXs) (C.FuncCallStd "size" []) ))
          (C.Op1Prefix "++" (C.VarExpr varI))
          [ C.StmtExpr $ C.Op2Infix "=" (C.ArrayAccess (C.VarExpr varXs) (C.VarExpr varI)) (C.VarExpr varI)
          ] , 
        C.StmtExpr $ C.FuncCallUsr (mkName "calc") [rawPtr varXs, rawPtr varYs],
        C.StmtFor 
          (C.VarDefCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.MemberAccess (C.VarExpr varYs) (C.FuncCallStd "size" []) ))
          (C.Op1Prefix "++" (C.VarExpr varI))
          [ C.StmtExpr   $ cout << C.ArrayAccess (C.VarExpr varYs) (C.VarExpr varI) << endl
          ] , 
        C.StmtReturn $ intImm 0 ]

    calcBody = 
      [ C.StmtFor 
          (C.VarDefCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.VarExpr varN) )
          (C.Op1Prefix "++" (C.VarExpr varI))
          [ C.StmtExpr $ C.Op2Infix "=" (C.ArrayAccess (C.VarExpr varPY) (C.VarExpr varI)) 
            (C.Op2Infix "*" (C.ArrayAccess (C.VarExpr varPX) (C.VarExpr varI))
                            (C.ArrayAccess (C.VarExpr varPX) (C.VarExpr varI)) )
          ] , 
       C.StmtReturn $ C.toDyn () 
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    intImm :: Int -> C.Expr
    intImm = C.toDyn

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)

    tVoid :: C.TypeRep
    tVoid = C.typeOf ()

    tVecInt :: C.TypeRep
    tVecInt = C.TemplateType "std::vector" [tInt]

    tV2Int :: C.TypeRep
    tV2Int = C.TemplateType "std::vector" [tVecInt]

