#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Dynamic
import           Data.Tensor.TypeLevel
import qualified Data.Text.IO as T
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import qualified Language.Paraiso.Annotation.SyncThreads as Sync
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.OM.Realm 
import           Language.Paraiso.Optimization
import           Language.Paraiso.Prelude 
import qualified Language.Paraiso.Tuning.Genetic as GA
import           NumericPrelude hiding ((++))
import           System.Process (system)

bind = fmap return
loadLD = load TLocal (undefined::Double)

proceed :: Builder Vec1 Int Anot.Annotation ()
proceed = do 
  --x <- bind $ Anot.add Sync.Pre <?> loadLD density
  --y <- bind $ Anot.add Alloc.Manifest <?> x * x
  x <- bind $ loadLD density
  y <- bind $ x * x
  z <- bind $ y + y
  store density z

density :: Name
density = mkName "density"

myVars :: [Named DynValue]
myVars = [Named density DynValue{DVal.realm = Local, typeRep = typeOf (0::Double)}]

myOM :: OM Vec1 Int Anot.Annotation
myOM = optimize O3 $
  makeOM (mkName "Hello") [] myVars
  [(mkName "proceed", proceed)]

mySetup :: Native.Setup Vec1 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 256)
  { Native.directory = "./dist/", 
    Native.language = Native.CUDA
  }


main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  let originalSpecies = GA.makeSpecies mySetup myOM
      originalDNA     = GA.readGenome $ originalSpecies
  T.writeFile "output/original.dna" $ (++"\n") $ showT $ originalDNA

  return ()