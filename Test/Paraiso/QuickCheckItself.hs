{-# OPTIONS -Wall #-}
module Test.Paraiso.QuickCheckItself
    (
     testQuickCheckItself
    )where

import Test.Paraiso.Adaptor     (testResult)
import Test.Framework   (Test, testGroup)
import Test.QuickCheck

testQuickCheckItself :: Test
testQuickCheckItself = testGroup "test for QuickTest itself" $ tests
  where
    tests = [
      makeTest "length test A" ((<=5) . length . first5Vowel) getStringWithA, 
      makeTest "vowel test A" (all (`elem` vowels) . first5Vowel) getStringWithA,
      makeTest "length test ~A" ((<=5) . length . first5Vowel) getStringOhneA, 
      makeTest "vowel test ~A" (all (`elem` vowels) . first5Vowel) getStringOhneA,
      makeTest "length test C" ((==0) . length . first5Vowel) getConsonants] 
    makeTest name test provider = 
      testResult name $ quickCheckWithResult stdArgs{maxSize=1000, chatty=False} $ test . provider 
                               

vowels :: String
vowels = "AIUEOaiueo"

consonants :: String
consonants = filter (not . (`elem` vowels)) [' '..'~']

first5Vowel :: String -> String
first5Vowel = take 5 . filter (`elem` vowels)


newtype StringWithA = StringWithA {getStringWithA :: String} deriving  Show
newtype StringOhneA = StringOhneA {getStringOhneA :: String} deriving  Show
newtype Consonants  = Consonants  {getConsonants  :: String} deriving  Show

instance Arbitrary StringWithA where
  arbitrary = fmap StringWithA $ arbitrary `suchThat` ('a' `elem`)

instance Arbitrary StringOhneA where
  arbitrary = fmap StringOhneA $ arbitrary `suchThat` (not . ('a' `elem`))

instance Arbitrary Consonants where
  arbitrary = fmap Consonants $ listOf . elements $ consonants


