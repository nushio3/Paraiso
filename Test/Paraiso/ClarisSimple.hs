{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.ClarisSimple (
  test
  )where

import           Data.Dynamic
import qualified Language.Paraiso.Generator.Claris as C
import           Language.Paraiso.Name      (mkName)
import           Language.Paraiso.Prelude
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
    myArgs = stdArgs{maxSuccess = 10, maxDiscard = 100,maxSize=1000, chatty=False}

data AdderQuiz = AdderQuiz {adderQuizAns :: Int, adderProg :: C.Program, progText :: Text} deriving Show
instance Arbitrary AdderQuiz where
  arbitrary = flip fmap arbitrary $
              (\(x',y') -> 
                let [x,y] = map (`div` 65536) [x', y'] 
                    prog = adderProgram x y
                in
                 AdderQuiz{ 
                   adderProg    = prog,
                   adderQuizAns = x+y,
                   progText     = C.translate C.sourceFile prog
                   }
              ) 

testQuiz :: AdderQuiz -> Bool
testQuiz (AdderQuiz ans prog _) = ans == evaluate prog

adderProgram :: Int -> Int -> C.Program
adderProgram x1 x2 = 
  C.Program {
    C.progName = mkName "hello",
    C.topLevel = 
      [C.PrprInst $ C.Include C.SourceFile C.Chevron "iostream",
       C.FuncDecl $ (C.function tInt (mkName "main")){C.funcBody = body}]
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
    
