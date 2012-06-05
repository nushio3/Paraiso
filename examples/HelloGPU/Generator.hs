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
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Value (StaticValue(..))
import           Language.Paraiso.OM.PrettyPrint (prettyPrintA1)
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
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
-- the setup for CUDA code generation.
mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 10 :~ 20)
  { Native.directory = "./dist/" ,
    Native.language  = Native.CUDA,
    Native.cudaGridSize = (32,1)
  }

-- the orthotope machine to be generated
myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "TableMaker") [] myVars myKernels

-- the variables we use
table :: Named (StaticValue TArray Int)
table = "table" `isNameOf` StaticValue TArray  undefined

total :: Named (StaticValue TScalar Int)
total = "total" `isNameOf` StaticValue TScalar undefined

myVars :: [Named DynValue]
myVars = [f2d table, f2d total]
    
-- the only kernel our OM has
myKernels :: [Named (Builder Vec2 Int Annotation ())]
myKernels = ["create" `isNameOf` createBuilder]

-- the kernel builder monad
createBuilder :: Builder Vec2 Int Annotation ()
createBuilder = do 
  x <- bind $ loadIndex (Axis 0) 
  y <- bind $ loadIndex (Axis 1) 
  z <- bind $ x*y
  store table z
  store total $ reduce Reduce.Sum z

