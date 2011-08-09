{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Dynamic
import           Language.Paraiso.Generator (generate)
import qualified Language.Paraiso.Generator.Claris as C
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude


main :: IO ()
main = do
  _ <- generate (adderProgram 20 22) "./" 
  return ()

adderProgram :: Int -> Int -> C.Program
adderProgram x1 x2 = 
  C.Program {
    C.progName = mkName "main",
    C.topLevel = 
      [C.PrprInst $ C.Include C.SourceFile C.Chevron "iostream" ,
       C.FuncDecl $ C.Function (mkName "main") [] tInt [] body]
    }
  where
    body = 
      [C.StmtExpr coutExpr,
       C.StmtReturn $ C.Imm $ toDyn (0::Int) ]
      
    coutExpr = cout << message << endl

    cout = C.VarExpr $ C.Var C.unknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.unknownType $ mkName "std::endl"
    
    message = C.Op2Infix "+" (C.Imm $ toDyn x1) (C.Imm $ toDyn x2)

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    tInt :: TypeRep
    tInt = typeOf (undefined :: Int)
