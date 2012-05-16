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
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.Optimization
import           NumericPrelude
import           System.Process (system)

bind :: (Monad m, Functor m) => m a -> m (m a)
bind = fmap return

initialize :: Builder Vec1 Int Annotation ()
initialize = do
  i <- bind $ loadIndex (0::Double) $ Axis 0  
  n <- bind $ loadSize TLocal (0::Double) $ Axis 0  
  x <- bind $  2 * pi / n * (i + 0.5)
  store fieldF $ sin(x)
  store fieldG $ cos(3*x)
  
  
proceed :: Builder Vec1 Int Annotation ()
proceed = do 
  c  <- bind $ imm 3.43
  n  <- bind $ loadSize TLocal (0::Double) $ Axis 0
  
  f0 <- bind $ load TLocal (0::Double) $ fieldF
  g0 <- bind $ load TLocal (0::Double) $ fieldG
  dx <- bind $ 2 * pi / n
  dt <- bind $ dx / c

  f1 <- bind $ f0 + dt * g0
  fR <- bind $ shift (Vec:~ -1) f1 
  fL <- bind $ shift (Vec:~  1) f1 
  g1 <- bind $ g0 + dt * c^2 / dx^2 * 
        (fL + fR - 2 * f1)
  store fieldF f1
  store fieldG g1
  
  dfdx <- bind $ (fR - fL) / (2*dx)
  store energy $ reduce Reduce.Sum $
    0.5 * (c^2 * dfdx^2 + ((g0+g1)/2)^2) * dx

fieldF, fieldG, energy :: Name
fieldF = mkName "fieldF"
fieldG = mkName "fieldG"
energy = mkName "energy"


myVars :: [Named DynValue]
myVars = [Named fieldF DynValue{DVal.realm = Local,  typeRep = typeOf (0::Double)}, 
          Named fieldG DynValue{DVal.realm = Local,  typeRep = typeOf (0::Double)},
          Named energy DynValue{DVal.realm = Global, typeRep = typeOf (0::Double)}
          ]

myOM :: OM Vec1 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "LinearWave") [] myVars
  [(mkName "initialize", initialize), 
   (mkName "proceed", proceed)]

mySetup :: Int -> Native.Setup Vec1 Int
mySetup n = 
  (Native.defaultSetup $ Vec :~ n)
  { Native.directory = "./dist/" 
  }


mainTest :: Int -> IO ()
mainTest n = do
  _ <- system "mkdir -p output"
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  _ <- generateIO (mySetup n) myOM 
  _ <- system "make"
  _ <- system "./a.out"
  return ()
  
main :: IO ()  
main = mapM_  mainTest $
       concat $ [[2*2^n, 3*2^n]| n <- [2..10]]
  
  