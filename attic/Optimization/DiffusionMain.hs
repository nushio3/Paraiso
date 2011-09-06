{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import qualified Data.Text.IO as T
import           Data.Typeable
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import           Language.Paraiso.Name
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean
import qualified Language.Paraiso.OM.Value as Val
import           Language.Paraiso.OM.DynValue 
import           Language.Paraiso.OM
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.Optimization
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

type Real = Double
type Dim = Vec3
type B ret = Builder Dim Int Anot.Annotation ret
type BR = B (Val.Value Rlm.TLocal Real)
type BGR = B (Val.Value Rlm.TGlobal Real)


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
      [Named (mkName "pink") doubleDV] ++
      [Named (mkName "black") doubleDV]       


-- adjacency vectors in Conway's game of Life
adjVecs :: [Dim Int]
adjVecs = zipWith3 (\x y z -> Vec :~ x :~ y :~ z)
          [-1, 1, 0, 0, 0, 0]
          [ 0, 0,-1, 1, 0, 0]
          [ 0, 0, 0, 0,-1, 1]

r5mino :: [Dim Int]
r5mino = zipWith3 (\x y z -> Vec :~ x :~ y :~ z)
         [ 1, 2, 0, 1, 1]
         [ 0, 0, 1, 1, 2]
         [42,44,45,46,42]

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
    
       
diffuse :: BR -> B BR
diffuse field = do
  -- create a list of field, each shifted by an element of adjVects.
  neighbours <- fmap (map return) $
                forM adjVecs (\v -> shift v field)
  -- add them all.
  num <- bind $ foldl1 (+) neighbours
  -- create the new cell state based on the judgement.
  --ret <- bind $ (Anot.add Alloc.Manifest <?> (1/6) * num )
  ret <- bind $ ((1/6) * num )  
  return $ ret 
  
       
buildProceed :: B ()
buildProceed = do
  -- load a Local variable called "cell."
  pink  <- bind $ load Rlm.TLocal  (undefined::Real) $ mkName "pink"
  black <- bind $ load Rlm.TLocal  (undefined::Real) $ mkName "black"
  
  -- load a Global variable called "generation."
  gen  <- bind $ load Rlm.TGlobal (undefined::Int) $ mkName "generation"
  
                
  pink1  <- diffuse black
  black1 <- diffuse pink
  
  pink2  <- diffuse black1
  black2 <- diffuse pink1

  pink3  <- diffuse black2
  black3 <- diffuse pink2

  -- count the number of alive cells and store it into "population."
  store (mkName "sum") $ reduce Reduce.Sum pink3
  
  -- increment the generation.
  store (mkName "generation") $ gen + 1
  
  
  store (mkName "pink") $ pink3
  store (mkName "black") $ black3


buildInit :: B ()
buildInit = do
  -- create the current coordinate vector.
  coord <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Int) axis)
  
  -- if the current coordinate equals one of the elements in r5mino, you are alive.
  alive <- bind $ foldl1 (||) [agree coord point | point <- r5mino ]
  
  -- create the initial cell state based on the judgement.
  cell  <- bind $ select alive (10000::BuilderOf Rlm.TLocal Real) 0
  
  -- store the initial states.
  store (mkName "pink") $ cell
  store (mkName "black") $ cell  
  store (mkName "sum") $ reduce Reduce.Sum cell
  store (mkName "generation") $ (0::BuilderOf Rlm.TGlobal Int) 
  
  where
    agree coord point = 
      foldl1 (&&) $ compose (\i -> coord!i `eq` imm (point!i))

-- compose the machine.
myOM :: OM Dim Int Anot.Annotation
myOM = optimize O3 $ 
  makeOM (mkName "diffusion") [] lifeVars
    [ (mkName "init"   , buildInit),
      (mkName "proceed", buildProceed)
    ]
              

genSetup :: Native.Setup Dim Int
genSetup 
  = (Native.defaultSetup $ Vec :~ 128  :~ 128  :~ 128)
  { Native.directory = "./dist/" }

genSetup2 :: Native.Setup Dim Int
genSetup2 
  = genSetup 
  { Native.language = Native.CUDA, 
    Native.directory = "./dist2/" 
  }


main :: IO ()
main = do
  -- output the intermediate state.
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM
  
  -- generate the library 
  _ <- generateIO genSetup myOM
  _ <- generateIO genSetup2 myOM  
  
  return ()
