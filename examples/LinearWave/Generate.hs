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

bind :: (Monad m, Functor m) => m a -> m (m a)
bind = fmap return

proceed :: Builder Vec1 Int Annotation ()
proceed = do 
  f0 <- bind $ load TLocal (0::Double) $ fieldF
  g0 <- bind $ load TLocal (0::Double) $ fieldG
  c  <- bind $ imm 1.0
  n  <- bind $ loadSize TLocal (0::Double) $ Axis 0
  dx <- bind $ 2 * pi / n
  dt <- bind $ dx / c
  
  f1 <- bind $ f0 + dt * g0
  g1 <- bind $ g0 + dt * c**2 / dx**2 * 
        (shift (Vec:~ -1) f1 + 
         shift (Vec:~  1) f1 -
         2* f1)
  store fieldF f1
  store fieldG g1

fieldF, fieldG :: Name
fieldF = mkName "fieldF"
fieldG = mkName "fieldG"


myVars :: [Named DynValue]
myVars = [Named fieldF DynValue{DVal.realm = Local, typeRep = typeOf (0::Double)}, 
          Named fieldG DynValue{DVal.realm = Local, typeRep = typeOf (0::Double)}
          ]

myOM :: OM Vec1 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "LinearWave") [] myVars
  [(mkName "proceed", proceed)]

mySetup :: Native.Setup Vec1 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 256)
  { Native.directory = "./dist/" 
  }


main :: IO ()
main = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO mySetup myOM
  return ()