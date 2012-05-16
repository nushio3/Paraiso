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
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.OM.Realm 
import           Language.Paraiso.Optimization
import           NumericPrelude
import           System.Process (system)

bind = fmap return

buildProceed :: Builder Vec2 Int Annotation ()
buildProceed = do 
  x <- bind $ load TGlobal (undefined::Int) $ cellName
  y <- bind $ x * x
  z <- bind $ y + y
  store cellName z

cellName :: Name
cellName = mkName "someCalc"

myVars :: [Named DynValue]
myVars = [Named cellName DynValue{DVal.realm = Global, typeRep = typeOf (0::Int)}]

myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "Hello") [] myVars
  [(mkName "proceed", buildProceed)]

mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 256 :~ 256)
  { Native.directory = "./dist/" 
  }


main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  return ()