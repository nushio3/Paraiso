#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Dynamic
import           Data.Tensor.TypeLevel
import qualified Data.Text.IO as T
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.PrettyPrint (prettyPrintA1)
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.Optimization
import           NumericPrelude
import           System.Process (system)

-- the names we use
table, total, create, tableMaker :: Name
table = mkName "table"
total = mkName "total"
create = mkName "create"
tableMaker = mkName "TableMaker"


main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  return ()

mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 10 :~ 20)
  { Native.directory = "./dist/" ,
    Native.language  = Native.CUDA,
    Native.cudaGridSize = (32,1)
  }

myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM tableMaker [] myVars myKernels

myVars :: [Named DynValue]
myVars = [Named table $ DynValue Array (typeOf (undefined::Int)),
          Named total $ DynValue Scalar (typeOf (undefined::Int))]

myKernels :: [Named (Builder Vec2 Int Annotation ())]
myKernels = [Named create createBuilder]

createBuilder :: Builder Vec2 Int Annotation ()
createBuilder = do 
  x <- bind $ loadIndex (undefined::Int) (Axis 0) 
  y <- bind $ loadIndex (undefined::Int) (Axis 1) 
  z <- bind $ x*y
  store table z
  store total $ reduce Reduce.Sum z

