{-# OPTIONS -Wall #-}
module Test.Paraiso.Claris (
  testClaris
  ) where

import           Test.Framework            (Test, testGroup)       
import qualified Test.Paraiso.ClarisSimple
import           Test.QuickCheck

testClaris :: Test
testClaris = testGroup "Claris" 
  [ Test.Paraiso.ClarisSimple.test ]

