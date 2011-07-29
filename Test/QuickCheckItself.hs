{-# OPTIONS -Wall #-}
module Test.QuickCheckItself
    (
     testQuickCheckItself
    )where

import Test.Framework  (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.API (TestName)
import Test.QuickCheck
import Test.HUnit.Lang (assertFailure)

testQuickCheckItself :: Test
testQuickCheckItself = testResult "length test" $
  quickCheckWithResult stdArgs{maxSize=1000, chatty=False} $ 
    (<=5) . length . first5Vowel . getStringWithA

vowels :: String
vowels = "AIUEOaiueo"

first5Vowel :: String -> String
first5Vowel = take 5 . filter (`elem` vowels)


newtype StringWithA = StringWithA {getStringWithA :: String}
    deriving (Eq, Show)

instance Arbitrary StringWithA where
    arbitrary = fmap StringWithA $ arbitrary `suchThat` ('a' `elem`)



testResult :: TestName -> IO Result -> Test
testResult testName qTest = testCase testName $ do
  ret <- qTest
  case ret of
    Success _ _ _ -> return ()
    _ -> assertFailure $ output ret





