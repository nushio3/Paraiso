#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Control.Monad
import qualified Data.Text.IO as T
import           Data.Tensor.TypeLevel
import           Language.Paraiso.Annotation (Annotation)
import qualified Language.Paraiso.Annotation.Boundary as Boundary
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean (select,eq,ge,le)
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.OM.Value (StaticValue(..))
import           Language.Paraiso.Optimization
import           Language.Paraiso.Prelude
import           NumericPrelude hiding ((||),(&&))


-- the main program
main :: IO ()
main = do
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  
  _ <- generateIO mySetup myOM
  _ <- generateIO 
       mySetup{ Native.language = Native.CUDA 
              , Native.directory = "./dist-cuda/"}
       myOM  
  return ()

-- the code generation setup
mySetup :: Native.Setup Vec2 Int
mySetup = 
  (Native.defaultSetup $ Vec :~ 80 :~ 48)
  { Native.directory = "./dist/" ,
    Native.boundary = compose $ const Boundary.Cyclic
  }

-- the orthotope machine to be generated
myOM :: OM Vec2 Int Annotation
myOM = optimize O3 $
  makeOM (mkName "Life") [] myVars myKernels

-- the variables we use
cell :: Named (StaticValue TArray Int)
cell = "cell" `isNameOf` StaticValue TArray  undefined

population :: Named (StaticValue TScalar Int)
population = "population" `isNameOf` StaticValue TScalar undefined

generation :: Named (StaticValue TScalar Int)
generation = "generation" `isNameOf` StaticValue TScalar undefined

-- the list of variables we use.
-- we need to dynamize every variable to put them into a same list.
myVars :: [Named DynValue]
myVars = [f2d cell, f2d population, f2d generation]
    
-- our kernel
myKernels :: [Named (Builder Vec2 Int Annotation ())]
myKernels = ["init" `isNameOf` initBuilder,
             "proceed" `isNameOf` proceedBuilder]

initBuilder :: Builder Vec2 Int Annotation ()
initBuilder = do 
  -- store the initial states.
  store cell 0
  store population 0
  store generation 0

-- adjacency vectors in Conway's game of Life
adjVecs :: [Vec2 Int]
adjVecs = zipWith (\x y -> Vec :~ x :~ y)
          [-1, 0, 1,-1, 1,-1, 0, 1]
          [-1,-1,-1, 0, 0, 1, 1, 1]


proceedBuilder :: Builder Vec2 Int Annotation ()
proceedBuilder = do 
    -- load a Array variable called "cell."
  oldCell <- bind $ load cell

  -- load a Scalar variable called "generation."
  gen  <- bind $ load generation

  -- create a list of cell patterns, each shifted by an element of adjVects.
  neighbours <- forM adjVecs $
    \v -> bind $ shift v oldCell

  -- add them all.
  num <- bind $ sum neighbours

  -- The rule of Conway's game of Life.
  isAlive <- bind $
             (oldCell `eq` 0) && (num `eq` 3) ||
             (oldCell `eq` 1) && (num `ge` 2) && (num `le` 3) 

  -- create the new cell state based on the judgement.
  newCell <- bind $ select isAlive 1 0

  -- count the number of alive cells and store it into "population."
  store population $ reduce Reduce.Sum newCell

  -- increment the generation.
  store generation $ gen + 1

  -- store the new cell state.
  store cell $ newCell

