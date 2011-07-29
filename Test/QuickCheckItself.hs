{-# OPTIONS -Wall #-}
module Test.QuickCheckItself
    (
     testQuickCheckItself
    )where

import Test.Framework  (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit.Lang (assertFailure)

testQuickCheckItself :: Test
testQuickCheckItself = testCase "length test" testBody

vowels :: String
vowels = "AIUEOaiueo"

first5Vowel :: String -> String
first5Vowel = take 5 . filter (`elem` vowels)


newtype StringWithA = StringWithA {getStringWithA :: String}
    deriving (Eq, Show)

instance Arbitrary StringWithA where
    arbitrary = fmap StringWithA $ arbitrary `suchThat` ('a' `elem`)


testBody :: IO ()
testBody = do
  ret <- quickCheckWithResult stdArgs{maxSize=1000, chatty=False} $ 
         (<=5) . length . first5Vowel . getStringWithA
  case ret of
    Success _ _ _ -> return ()
    _ -> assertFailure $ output ret

