{-# LANGUAGE  NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import Data.Typeable
--import qualified Data.Graph.Inductive as FGL
import Language.Paraiso.Annotation (Annotation)
import Language.Paraiso.OM.Builder
import Language.Paraiso.OM.Builder.Boolean
import Language.Paraiso.OM.DynValue 
import Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import Language.Paraiso.POM
import Language.Paraiso.Prelude
import Language.Paraiso.Tensor


-- a dynamic representation for a local static value (an array)
intDV :: DynValue
intDV = DynValue{realm = Rlm.Local, typeRep = typeOf (0::Int)}

-- a dynamic representation for a global static value (a single-point variable)
intGDV :: DynValue
intGDV = DynValue{realm = Rlm.Global, typeRep = typeOf (0::Int)}


-- the list of static variables for this machine
lifeSetup :: Setup Vec2 Int 
lifeSetup = Setup $ 
            [Named (Name "population") intGDV] ++
            [Named (Name "generation") intGDV] ++
            [Named (Name "cell") intDV] 


-- adjacency vectors in Conway's game of Life
adjVecs :: [Vec2 Int]
adjVecs = zipWith (\x y -> Vec :~ x :~ y)
          [-1, 0, 1,-1, 1,-1, 0, 1]
          [-1,-1,-1, 0, 0, 1, 1, 1]

-- R-pentomino pattern
r5mino :: [Vec2 Int]
r5mino = zipWith (\x y -> Vec :~ x :~ y)
         [ 1, 2, 0, 1, 1]
         [ 0, 0, 1, 1, 2]

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
    
       
       
buildProceed :: Builder Vec2 Int ()
buildProceed = do
  -- load a Local variable called "cell."
  cell <- bind $ load Rlm.TLocal  (undefined::Int) $ Name "cell"
  
  -- load a Global variable called "generation."
  gen  <- bind $ load Rlm.TGlobal (undefined::Int) $ Name "generation"
  
  -- create a list of cell patterns, each shifted by an element of adjVects.
  neighbours <- fmap (map return) $
                forM adjVecs (\v -> shift v cell)
                
  -- add them all.
  num <- bind $ foldl1 (+) neighbours
  
  -- The rule of Conway's game of Life.
  isAlive <- bind $
             (cell `eq` 0) && (num `eq` 3) ||
             (cell `eq` 1) && (num `ge` 2) && (num `le` 3) 
             
  -- create the new cell state based on the judgement.
  newCell <- bind $ select isAlive (1::BuilderOf Rlm.TLocal Int) 0
  
  -- count the number of alive cells and store it into "population."
  store (Name "population") $ reduce Reduce.Sum newCell
  
  -- increment the generation.
  store (Name "generation") $ gen + 1
  
  -- store the new cell state.
  store (Name "cell") $ newCell


buildInit :: Builder Vec2 Int ()
buildInit = do
  -- create the current coordinate vector.
  coord <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Int) axis)
  
  -- if the current coordinate equals one of the elements in r5mino, you are alive.
  alive <- bind $ foldl1 (||) [agree coord point | point <- r5mino ]
  
  -- create the initial cell state based on the judgement.
  cell  <- bind $ select alive (1::BuilderOf Rlm.TLocal Int) 0
  
  -- store the initial states.
  store (Name "cell") $ cell
  store (Name "population") $ reduce Reduce.Sum cell
  store (Name "generation") $ (0::BuilderOf Rlm.TGlobal Int) 
  
  where
    agree coord point = 
      foldl1 (&&) $ compose (\i -> coord!i `eq` imm (point!i))

buildShiftX :: Builder Vec2 Int ()
buildShiftX = do
  store (Name "cell") $  
    shift (Vec :~ 1 :~ 0) $
    load Rlm.TLocal  (undefined::Int) $ Name "cell"

buildShiftY :: Builder Vec2 Int ()
buildShiftY = do
  store (Name "cell") $  
    shift (Vec :~ 0 :~ 1) $
    load Rlm.TLocal  (undefined::Int) $ Name "cell"


-- compose the machine.
pom :: POM Vec2 Int Annotation
pom = 
  makePOM (Name "Life")  lifeSetup
    [(Name "init"   , buildInit),
     (Name "proceed", buildProceed),
     (Name "shift_x", buildShiftX),
     (Name "shift_y", buildShiftY)
     ]
              

main :: IO ()
main = do
  -- output the intermediate state.
  writeFile "output/POM.txt" $ show pom ++ "\n"
  
  -- one day, you will be able to generate the library again....
  -- generate Cpp pom "dist"
 


  
