{-# OPTIONS -Wall #-}
module Test.Adaptor
    (
     testResult
    )where

import Test.Framework                 (Test)
import Test.Framework.Providers.API   (TestName)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Lang                (assertFailure)
import Test.QuickCheck                (Result(..))

testResult :: TestName -> IO Result -> Test
testResult testName qTest = testCase testName $ do
  ret <- qTest
  case ret of
    Success _ _ _ -> return ()
    _ -> assertFailure $ output ret





