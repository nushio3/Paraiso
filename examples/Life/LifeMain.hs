{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import Control.Monad.State
import Data.Typeable
import Data.Traversable (sequenceA)
import Data.Foldable (foldl1)
import qualified Data.Graph.Inductive as FGL
import Language.Paraiso.Tensor
import Language.Paraiso.OM.Builder
import Language.Paraiso.OM.Builder.Boolean
import Language.Paraiso.OM.DynValue 
import Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import Language.Paraiso.Prelude hiding (foldl1)



intDV :: DynValue
intDV = DynValue{realm = Rlm.Local, typeRep = typeOf (0::Int)}

intGDV :: DynValue
intGDV = DynValue{realm = Rlm.Global, typeRep = typeOf (0::Int)}



lifeSetup :: Setup Vec2 Int
lifeSetup = Setup $ 
            [NamedValue (Name "population") intGDV] ++
            [NamedValue (Name "generation") intGDV] ++
            [NamedValue (Name "cell") intDV] 


adjVecs :: [Vec2 Int]
adjVecs = zipWith (\x y -> Vec :~ x :~ y)
          [-1, 0, 1,-1, 1,-1, 0, 1]
          [-1,-1,-1, 0, 0, 1, 1, 1]

r5mino :: [Vec2 Int]
r5mino = zipWith (\x y -> Vec :~ x :~ y)
         [ 0, 0, 1, 1, 2]
         [ 1, 2, 0, 1, 1]

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
    
buildProceed :: Builder Vec2 Int ()
buildProceed = do
  cell <- bind $ load Rlm.TLocal  (undefined::Int) $ Name "cell"
  gen  <- bind $ load Rlm.TGlobal (undefined::Int) $ Name "generation"
  neighbours <- fmap (map return) $
                forM adjVecs (\v -> shift v cell)
  num <- bind $ foldl1 (+) neighbours
  isAlive <- bind $
             (cell `eq` 0) && (num `eq` 3) ||
             (cell `eq` 1) && (num `ge` 2) && (num `le` 3) 
  store (Name "population") $ reduce Reduce.Sum cell
  store (Name "generation") $ gen + 1
  store (Name "cell") $ select isAlive (1::BuilderOf Rlm.TLocal Int) 0


buildInit :: Builder Vec2 Int ()
buildInit = do
  coord <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Int) axis)
  alive <- bind $ foldl1 (||) [agree coord point | point <- r5mino ]
  store (Name "cell") $ select alive (1::BuilderOf Rlm.TLocal Int) 0
  
  where
    agree coord point = 
      foldl1 (&&) $ compose (\i -> coord!i `eq` imm (point!i))

main :: IO ()
main = do
  putStrLn "hi"
  let state0 = initState lifeSetup
  print $ state0
  writeFile "output/Init.hs" $ show $ FGL.grev $ target $ snd $ runState buildInit state0
  writeFile "output/Proceed.hs" $ show $ FGL.grev $ target $ snd $ runState buildProceed state0
  
  
