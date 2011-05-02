{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
import Control.Monad.State
import qualified Data.List as List
import Data.Typeable
import Language.Paraiso.Tensor
import Language.Paraiso.OM.Builder
import Language.Paraiso.OM.Builder.Boolean
import Language.Paraiso.OM.DynValue 
import Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import Language.Paraiso.Prelude



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


bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
    
buildProceed :: Builder Vec2 Int ()
buildProceed = do
  cell <- bind $ load Rlm.TLocal  (undefined::Int) $ Name "cell"
  gen  <- bind $ load Rlm.TGlobal (undefined::Int) $ Name "generation"
  neighbours <- fmap (map return) $
                forM adjVecs (\v -> shift v cell)
  num <- bind $ List.foldl1 (+) neighbours
  isAlive <- bind $
             (cell `eq` 0) && (num `eq` 3) ||
             (cell `eq` 1) && (num `ge` 2) && (num `le` 3) 
  store (Name "population") $ reduce Reduce.Sum cell
  store (Name "generation") $ gen + 1
  store (Name "cell") $ select isAlive (1::BuilderOf Int) 0


main :: IO ()
main = do
  putStrLn "hi"
  let state0 = initState lifeSetup
  print $ state0
  let 
      (_, s) = runState buildProceed state0
      g :: Graph Vec2 Int ()
      g = target s
  print $ g
  
  
