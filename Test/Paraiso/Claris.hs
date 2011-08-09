{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.Claris
    (
     testClaris
    )where

import           Control.Concurrent         (forkIO)
import           Data.Dynamic
import qualified Data.ListLike as LL
import           Language.Paraiso.Generator (generate)
import qualified Language.Paraiso.Generator.Claris as C
import           Language.Paraiso.Name      (mkName)
import           Language.Paraiso.Prelude
import           System.IO                  (hGetLine, hIsEOF, Handle)
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Process             (createProcess, CreateProcess(..),
                                             system, shell, StdStream(..), waitForProcess)
import           System.Random              (randomIO)
import           System.FilePath            ((</>))
import           Test.Framework             (Test, testGroup)
import           Test.Paraiso.Adaptor       (testResult)
import qualified Test.Paraiso.Option as Option
import           Test.QuickCheck

testClaris :: Test
testClaris = testGroup "testing programs directly generated from Claris" tests
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
    
evaluate :: C.Program -> Int    
evaluate prog = unsafePerformIO $ do
  key <- randomIO
  let path :: FilePath
      path = "/tmp/" ++ (show :: Int -> String) key 
      exeFn = path </> "dragon.out" -- roar!
  files <- generate prog path
  let cppFn :: FilePath
      cppFn = head $ filter (LL.isSuffixOf ".cpp") files
  _ <- system $ unwords [Option.cppc, "-O3",  cppFn, "-I", path,  "-o",  exeFn]
  (_, Just hout, _, handle) <- createProcess (shell exeFn) {std_out = CreatePipe}
  ret <- fmap (read :: String -> Int) $ hGetLine hout
  _ <- forkIO $ suckAll hout
  _ <- waitForProcess handle
  _ <- system $ "rm -fr " ++ path
  return ret
  

suckAll :: Handle -> IO ()
suckAll hdl = do
  eof <- hIsEOF hdl
  if eof 
    then return ()
    else hGetLine hdl >> suckAll hdl

