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
import           Language.Paraiso.Generator.Claris as C
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
                   progText     = C.translate prog
                   }
              ) 

testQuiz :: AdderQuiz -> Bool
testQuiz (AdderQuiz ans prog _) = ans == evaluate prog


adderProgram :: Int -> Int -> C.Program
adderProgram x1 x2 = 
  C.Program {
    progName = mkName "hello",
    topLevel = 
      [PragmaDecl $ PragmaInclude (mkName "iostream") False Chevron,
       FuncDecl $ Function (mkName "main") [] tInt [] body]
    }
  where
    body = 
      [StmtExpr coutExpr,
       StmtReturn $ Imm $ toDyn (0::Int) ]
      
    coutExpr = cout << message << endl

    cout = VarExpr $ Var unknownType $ mkName "std::cout"
    endl = VarExpr $ Var unknownType $ mkName "std::endl"
    
    message = Op2Infix "+" (Imm $ toDyn x1) (Imm $ toDyn x2)

    infixl 1 <<
    (<<) = Op2Infix "<<"


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
  _ <- system $ unwords [Option.cppc, "-O3",  cppFn,  "-o",  exeFn]
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

