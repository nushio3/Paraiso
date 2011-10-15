#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Tensor.TypeLevel
import           Language.Paraiso.Annotation (Annotation)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

buildProceed :: Builder Vec2 Int Annotation ()
buildProceed = return ()

myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "Hello") [] myVars
  [(mkName "proceed", buildProceed)]

main :: IO ()
main = do
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM