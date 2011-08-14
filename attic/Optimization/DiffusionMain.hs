{-# LANGUAGE  NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import qualified Data.Text.IO as T
import           Data.Typeable
import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.Name
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean
import           Language.Paraiso.OM.DynValue 
import           Language.Paraiso.OM
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.Optimization
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

type Real = Double

-- a dynamic representation for a local static value (an array)
doubleDV :: DynValue
doubleDV = DynValue{realm = Rlm.Local, typeRep = typeOf (0::Real)}

-- a dynamic representation for a global static value (a single-point variable)
doubleGDV :: DynValue
doubleGDV = DynValue{realm = Rlm.Global, typeRep = typeOf (0::Real)}

-- a dynamic representation for a global static value (a single-point variable)
intGDV :: DynValue
intGDV = DynValue{realm = Rlm.Global, typeRep = typeOf (0::Int)}


-- the list of static variables for this machine
lifeVars :: [Named DynValue]
lifeVars =
      [Named (mkName "sum") doubleGDV] ++
      [Named (mkName "generation") intGDV] ++
      [Named (mkName "cell") doubleDV] 


-- adjacency vectors in Conway's game of Life
adjVecs :: [Vec3 Int]
adjVecs = zipWith3 (\x y z -> Vec :~ x :~ y :~ z)
          [-1, 1, 0, 0, 0, 0]
          [ 0, 0,-1, 1, 0, 0]
          [ 0, 0, 0, 0,-1, 1]

r5mino :: [Vec3 Int]
r5mino = zipWith3 (\x y z -> Vec :~ x :~ y :~ z)
         [ 1, 2, 0, 1, 1]
         [ 0, 0, 1, 1, 2]
         [42,44,45,46,42]

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
    
       
       
buildProceed :: Builder Vec3 Int Anot.Annotation ()
buildProceed = do
  -- load a Local variable called "cell."
  cell <- bind $ load Rlm.TLocal  (undefined::Real) $ mkName "cell"
  
  -- load a Global variable called "generation."
  gen  <- bind $ load Rlm.TGlobal (undefined::Int) $ mkName "generation"
  
  -- create a list of cell patterns, each shifted by an element of adjVects.
  neighbours <- fmap (map return) $
                forM adjVecs (\v -> shift v cell)
                
  -- add them all.
  num <- bind $ foldl1 (+) neighbours
  
  -- create the new cell state based on the judgement.
  newCell <- bind $ num * 6
  
  -- count the number of alive cells and store it into "population."
  store (mkName "sum") $ reduce Reduce.Sum newCell
  
  -- increment the generation.
  store (mkName "generation") $ gen + 1
  
  -- store the new cell state.
  store (mkName "cell") $ newCell


buildInit :: Builder Vec3 Int  Anot.Annotation ()
buildInit = do
  -- create the current coordinate vector.
  coord <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Int) axis)
  
  -- if the current coordinate equals one of the elements in r5mino, you are alive.
  alive <- bind $ foldl1 (||) [agree coord point | point <- r5mino ]
  
  -- create the initial cell state based on the judgement.
  cell  <- bind $ select alive (10000::BuilderOf Rlm.TLocal Real) 0
  
  -- store the initial states.
  store (mkName "cell") $ cell
  store (mkName "sum") $ reduce Reduce.Sum cell
  store (mkName "generation") $ (0::BuilderOf Rlm.TGlobal Int) 
  
  where
    agree coord point = 
      foldl1 (&&) $ compose (\i -> coord!i `eq` imm (point!i))

-- compose the machine.
myOM :: OM Vec3 Int Anot.Annotation
myOM = optimize O3 $ 
  makeOM (mkName "diffusion") [] lifeVars
    [ (mkName "init"   , buildInit),
      (mkName "proceed", buildProceed)
    ]
              

genSetup :: Native.Setup
genSetup = Native.defaultSetup { Native.directory = "./dist/" }


main :: IO ()
main = do
  -- output the intermediate state.
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  
  -- generate the library 
  _ <- generateIO genSetup myOM
  
  return ()
  


  
