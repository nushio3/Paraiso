{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Claris as C
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

main :: IO ()
main = do
  _ <- generateIO Native.defaultSetup{Native.language = Native.CUDA} $ 
       sampleProgram 
  return ()

sampleProgram :: C.Program
sampleProgram = 
  C.Program {
    C.progName = mkName "thrust",
    C.topLevel = 
      [ include C.Chevron "iostream" ,
        include C.Chevron "vector" ,
        include C.Chevron "thrust/device_vector.h" ,
        include C.Chevron "thrust/host_vector.h" ,
        C.Exclusive C.SourceFile $ C.StmtExpr $ C.VarDefSub varN (intImm 101),
        C.Exclusive C.SourceFile $ C.StmtExpr $ C.VarDefSub varNB (intImm 1),
        C.Exclusive C.SourceFile $ C.StmtExpr $ C.VarDefSub varNT (intImm 32),
        C.FuncDef $ (C.function tInt (mkName "main"))
          { C.funcBody= mainBody }, 
        C.FuncDef $ (C.function (C.QualifiedType [C.CudaGlobal] tVoid) (mkName "calc"))
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
    
    varN  = C.Var (C.Const tInt) (mkName "N")
    varNB = C.Var (C.Const tInt) (mkName "NB")
    varNT = C.Var (C.Const tInt) (mkName "NT")
    
    varHXs = C.Var tHostVecInt (mkName "hxs") 
    varHYs = C.Var tHostVecInt (mkName "hys") 
    varDXs = C.Var tDeviceVecInt (mkName "dxs") 
    varDYs = C.Var tDeviceVecInt (mkName "dys") 
    
    rawPtr xs = C.FuncCallStd "thrust::raw_pointer_cast" 
        [C.Op1Prefix "&*" $ C.MemberAccess (C.VarExpr xs) (C.FuncCallStd "begin" [])]
    
    -- thrust::raw_pointer_cast(&*xs.begin()),


    mainBody = 
      [ C.StmtExpr $ C.VarDefCon  varHXs (C.VarExpr varN),
        C.StmtExpr $ C.VarDefCon  varHYs (C.VarExpr varN),
        C.StmtExpr $ C.VarDefCon  varDXs (C.VarExpr varN),
        C.StmtExpr $ C.VarDefCon  varDYs (C.VarExpr varN),
        C.StmtFor 
          (C.VarDefCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.MemberAccess (C.VarExpr varHXs) (C.FuncCallStd "size" []) ))
          (C.Op1Prefix "++" (C.VarExpr varI))
          [ C.StmtExpr $ C.Op2Infix "=" (C.ArrayAccess (C.VarExpr varHXs) (C.VarExpr varI)) (C.VarExpr varI)
          ] , 
        C.StmtExpr $ C.Op2Infix "=" (C.VarExpr varDXs) (C.VarExpr varHXs) ,
        C.StmtExpr $ C.CudaFuncCallUsr (mkName "calc") (C.VarExpr varNB) (C.VarExpr varNT) 
          [rawPtr varDXs, rawPtr varDYs],
        C.StmtExpr $ C.Op2Infix "=" (C.VarExpr varHYs) (C.VarExpr varDYs) ,
        C.StmtFor 
          (C.VarDefCon varI (intImm 0))
          (C.Op2Infix "<" (C.VarExpr varI) (C.MemberAccess (C.VarExpr varHYs) (C.FuncCallStd "size" []) ))
          (C.Op1Prefix "++" (C.VarExpr varI))
          [ C.StmtExpr   $ cout << C.ArrayAccess (C.VarExpr varHYs) (C.VarExpr varI) << endl
          ] , 
        C.StmtReturn $ intImm 0 ]

    calcBody = 
      [ C.StmtFor 
          (C.VarDefCon varI threadIdxX)
          (C.Op2Infix "<" (C.VarExpr varI) (C.VarExpr varN) )
          (C.Op2Infix "+=" (C.VarExpr varI) blockDimX)
          [ C.StmtExpr $ C.Op2Infix "=" (C.ArrayAccess (C.VarExpr varPY) (C.VarExpr varI)) 
            (C.Op2Infix "*" (C.ArrayAccess (C.VarExpr varPX) (C.VarExpr varI))
                            (C.ArrayAccess (C.VarExpr varPX) (C.VarExpr varI)) )
          ] , 
       C.StmtReturn $ C.toDyn () 
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    threadIdxX = C.VarExpr $ C.Var C.UnknownType $ mkName "threadIdx.x"
    blockDimX  = C.VarExpr $ C.Var C.UnknownType $ mkName "blockDim.x" 
    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    intImm :: Int -> C.Expr
    intImm = C.toDyn

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)

    tVoid :: C.TypeRep
    tVoid = C.typeOf ()

    tHostVecInt :: C.TypeRep
    tHostVecInt = C.TemplateType "thrust::host_vector" [tInt]

    tDeviceVecInt :: C.TypeRep
    tDeviceVecInt = C.TemplateType "thrust::device_vector" [tInt]


