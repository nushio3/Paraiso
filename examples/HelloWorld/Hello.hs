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
import           Language.Paraiso.Optimization
import           NumericPrelude
import           System.Process (system)

varName, kernelName :: Name
varName = mkName "table"
kernelName = mkName "create"

create :: Builder Vec2 Int Annotation ()
create = do 
  x <- bind $ loadIndex (undefined::Int) (Axis 0) 
  y <- bind $ loadIndex (undefined::Int) (Axis 1) 
  z <- bind $ x*y
  store varName z

myVars :: [Named DynValue]
myVars = [Named varName $ DynValue Local (typeOf (undefined::Int))]

myKernels :: [Named (Builder Vec2 Int Annotation ())]
myKernels = [Named kernelName create]


myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "Hello") [] myVars myKernels

mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 10 :~ 10)
  { Native.directory = "./dist/" 
  }

main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  return ()