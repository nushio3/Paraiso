{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.Annotation
    (
     testAnnotation
    )where

import           Data.Dynamic
import           Data.Maybe    (fromJust)
import           Data.Text     (pack)
import           Language.Paraiso.Annotation (Annotation)
import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.Annotation.Balloon (Balloon(..))
import           Language.Paraiso.Annotation.Comment (Comment(..))
import           Test.Paraiso.Adaptor     (testResult)
import           Test.Framework   (Test, testGroup)
import           Test.QuickCheck

testAnnotation :: Test
testAnnotation = testGroup "test for Annotation" $ tests
  where
    tests = [
      makeTest "generation test" getRandomAnot $ (>=0) . length ,
      makeTest "uniqueness test" getChosenAnot $ (<=anotKindCount) . length ,
      makeTest "no contamination test" getRandomAnot $ (==(Nothing :: Maybe NeverAnot)) . Anot.toMaybe ,
      makeTest "insert test" getRandomAnot $
        (=="42") . getStringAnot . fromJust . Anot.toMaybe . Anot.set (StringAnot "42"),
      makeTest "unaffected insert test" getRandomAnot $
        (==42)   . getIntAnot . fromJust . Anot.toMaybe . Anot.add (StringAnot "42") . Anot.set (IntAnot 42),
      makeTest "unaffected insert test 2" getRandomAnot $
        (==True) . getBoolAnot . fromJust . Anot.toMaybe . Anot.set (StringAnot "True") . Anot.add (BoolAnot True)
      ]
    makeTest name provider test =
      testResult name $ quickCheckWithResult newArgs $ test . provider
    newArgs = stdArgs{maxSuccess = 1000, maxDiscardRatio = 10, maxSize=1000, chatty=False}


data NeverAnot = NeverAnot deriving (Eq, Show, Typeable)
newtype BoolAnot   = BoolAnot    {getBoolAnot   :: Bool}   deriving (Eq, Show, Typeable)
newtype IntAnot    = IntAnot     {getIntAnot    :: Int}    deriving (Eq, Show, Typeable)
newtype StringAnot = StringAnot  {getStringAnot :: String} deriving (Eq, Show, Typeable)

newtype RandomAnot = RandomAnot {getRandomAnot :: Annotation} deriving Show
newtype ChosenAnot = ChosenAnot {getChosenAnot :: Annotation} deriving Show

instance Arbitrary RandomAnot where
  arbitrary = fmap RandomAnot $ accumAnot True

instance Arbitrary ChosenAnot where
  arbitrary = fmap ChosenAnot $ accumAnot False


anotKindCount :: Int
accumAnot :: Bool -> Gen Annotation
(anotKindCount, accumAnot) =
  (length $ anotGen undefined,
   \mode -> fmap (foldl (flip ($)) Anot.empty) $ listOf $ oneof $ anotGen mode)
  where
    accum :: (Typeable a) => Bool -> a -> Annotation -> Annotation
    accum mode = if mode then Anot.add else Anot.set

    anotGen mode =
      [
        fmap (accum mode . BoolAnot)       arbitrary,
        fmap (accum mode . IntAnot)        arbitrary,
        fmap (accum mode . StringAnot)     arbitrary,
        fmap (accum mode . Balloon)       (arbitrary :: Gen Int),
        fmap (accum mode . Comment . pack) arbitrary
      ]
