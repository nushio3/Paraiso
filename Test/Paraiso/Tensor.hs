{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, TypeOperators  #-}
{-# OPTIONS -Wall #-}
module Test.Paraiso.Tensor
    (
     testTensor
    )where

import Language.Paraiso.Prelude
import Language.Paraiso.Tensor
import Test.Paraiso.Adaptor     (testResult)
import Test.Framework   (Test, testGroup)
import Test.QuickCheck

zero44 :: Vec4 (Vec4 Int)
zero44 = zero

testTensor :: Test
testTensor = testGroup "test for tensor" $ tests
  where
    tests = [
      makeTest "simple test" ((==0) . dimension :: Vec Int -> Bool) ,
      makeTest "dimension test" ((==4) . dimension :: Vec4 (Vec3 Double) -> Bool) ,
      makeTest "add zero test" (\x -> x + zero44 == x) ,
      makeTest "subtraction test" (\x -> zero44 - x == negate x) ,
      makeTest "negate test" (\x -> x + negate x == zero44) ,
      makeTest "multiply zero test" 
        (\x -> compose (\i -> contract (\j -> compose (\k -> x!i!j * zero44!j!k))) == zero44) ,
      makeTest "commutativity test" (\x y -> x + y == y + x && x - x == zero44) ,
      makeTest "associativity test" (\x y z-> x + (y+z) == (x+y) + z && x - x == zero44) ,
      makeTest "multiply notation test" 
        (\x y -> compose (\i -> contract (\j -> compose (\k -> x!i!j * y!j!k))) - 
                 compose (\a -> contract (\b -> compose (\c -> y!b!c * x!a!b))) + y + x 
                 == zero44 + x + y) 
      ]
    makeTest name test = 
      testResult name $ quickCheckWithResult stdArgs{maxSize=1000, chatty=False} $ test 


instance Arbitrary a => Arbitrary (Vec a) where
  arbitrary = elements [Vec]

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (n :~ a) where
  arbitrary = fmap (\(v, x) -> v :~ x) arbitrary