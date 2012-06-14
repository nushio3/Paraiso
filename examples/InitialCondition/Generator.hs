#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Tensor.TypeLevel
import qualified Data.Text.IO as T
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.DynValue (f2d, DynValue)
import           Language.Paraiso.OM.Value (StaticValue(..))
import           Language.Paraiso.OM.PrettyPrint (prettyPrintA1)
import           Language.Paraiso.OM.Realm 
import           Language.Paraiso.Optimization
import           NumericPrelude
import           System.Process (system)

-- the main program
main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  return ()

-- the code generation setup
mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 500 :~ 500)
  { Native.directory = "./dist/" 
  }

-- the orthotope machine to be generated
myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "TableMaker") [] myVars myKernels

-- the variables we use
table :: Named (StaticValue TArray Double)
table = "table" `isNameOf` StaticValue TArray  undefined

myVars :: [Named DynValue]
myVars = [f2d table]

-- the only kernel our OM has
myKernels :: [Named (Builder Vec2 Int Annotation ())]
myKernels = ["create" `isNameOf` createBuilder]

-- the kernel builder monad
createBuilder :: Builder Vec2 Int Annotation ()
createBuilder = do 
  x01 <- bind $ (cast $ loadIndex (Axis 0)) / (cast $ broadcast $ loadSize (Axis 0)) 
  y01 <- bind $ (cast $ loadIndex (Axis 1)) / (cast $ broadcast $ loadSize (Axis 1))  
  x <- bind $ 4 * (x01-0.5)
  y <- bind $ 5 * (y01-0.5)
  z <- bind $ atan((1- x^2 - (y-(x^2)**(1/3))^2)*10)
  store table z
