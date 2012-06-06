#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}


import           Data.Tensor.TypeLevel
import           Language.Paraiso.Annotation (Annotation)
import qualified Language.Paraiso.Annotation.Boundary as Boundary
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Value (StaticValue(..))
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.Optimization
import           NumericPrelude


-- the main program
main :: IO ()
main = do
  _ <- generateIO mySetup myOM
  _ <- generateIO
    mySetup{ Native.directory = "./dist-cyclic/", Native.boundary = Vec :~ Boundary.Cyclic}
    myOM
  return ()

-- the code generation setup
mySetup :: Native.Setup Vec1 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 8)
  { Native.directory = "./dist-open/" 
  }

-- the orthotope machine to be generated
myOM :: OM Vec1 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "TableMaker") [] myVars myKernels

-- the variables we use
table :: Named (StaticValue TArray Int)
table = "table" `isNameOf` StaticValue TArray  undefined

total :: Named (StaticValue TScalar Int)
total = "total" `isNameOf` StaticValue TScalar undefined

myVars :: [Named DynValue]
myVars = [f2d table, f2d total]
    
-- our kernel
myKernels :: [Named (Builder Vec1 Int Annotation ())]
myKernels = 
  ["init" `isNameOf` do
      store table $ loadIndex (Axis 0) 
      
  ,"increment" `isNameOf` do
      store table $ 1 + load table 
      
  ,"calculate" `isNameOf` do
      center <- bind $ load table
      right  <- bind $ shift (Vec :~ (-1)) center
      left   <- bind $ shift (Vec :~ ( 1)) center
      ret    <- bind $ 10000 * left + 100 * center + right
      store table ret
      store total $ reduce Reduce.Sum ret
  ]