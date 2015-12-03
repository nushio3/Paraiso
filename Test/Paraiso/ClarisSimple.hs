{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.ClarisSimple (
  test
  )where

import           Data.Dynamic
import qualified Language.Paraiso.Generator.Claris as C
import qualified Language.Paraiso.Generator.ClarisTrans as C
import           Language.Paraiso.Name      (mkName)
import           Language.Paraiso.Prelude
import           Prelude hiding ((++))
import           Test.Framework             (Test, testGroup)
import           Test.Paraiso.Adaptor       (testResult)
import qualified Test.Paraiso.Option as Option
import           Test.Paraiso.ClarisUtil
import           Test.QuickCheck

test :: Test
test = testGroup "simple program generated from Claris" tests
  where
    tests = if Option.cpp then tests1 else []
    tests1 = [
      testResult "adder test" $ quickCheckWithResult myArgs testQuiz
      ]
    myArgs = stdArgs{maxSuccess = 10, maxDiscardRatio = 10,maxSize=1000, chatty=False}

data AdderQuiz = AdderQuiz {adderQuizAns :: Int, adderProg :: C.Program, progText :: Text} deriving Show
instance Arbitrary AdderQuiz where
  arbitrary =
    flip fmap arbitrary $
    \(x',y') ->
      let [x,y] = map (`mod` 65536) [x', y']
          prog = adderProgram x y
      in
       AdderQuiz {
         adderProg    = prog,
         adderQuizAns = x+y,
         progText     = C.translate C.sourceFile prog
         }


testQuiz :: AdderQuiz -> Bool
testQuiz (AdderQuiz ans prog _) = ans == evaluate prog

adderProgram :: Int -> Int -> C.Program
adderProgram x1 x2 =
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
      [C.VarDefSub varZ (C.Imm $ toDyn(0::Int)),
       C.StmtExpr $ C.Op2Infix "+=" (C.VarExpr varZ) $
       C.Op2Infix "+" (C.VarExpr varX) (C.VarExpr varY),
       C.StmtReturn $ (C.VarExpr varZ)
      ]

    cout = C.VarExpr $ C.Var C.UnknownType $ mkName "std::cout"
    endl = C.VarExpr $ C.Var C.UnknownType $ mkName "std::endl"

    message = C.FuncCallUsr (mkName "calc") [C.toDyn x1, C.toDyn x2]

    infixl 1 <<
    (<<) = C.Op2Infix "<<"

    tInt :: C.TypeRep
    tInt = C.typeOf (undefined :: Int)
