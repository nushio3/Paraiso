{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Control.Monad.State
-- import qualified Data.Graph.Inductive as FGL
import Data.Typeable
import Language.Paraiso.Tensor
import Language.Paraiso.OM.Builder
import Language.Paraiso.OM.Builder.Boolean
import Language.Paraiso.OM.DynValue 
import Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm as Rlm
import qualified Language.Paraiso.OM.Reduce as Reduce
import NumericPrelude


tp :: TypeRep
tp = typeOf (0::Double)

dv :: DynValue
dv = DynValue{realm = Rlm.Local, typeRep = tp}

replicateV :: String -> Int -> [NamedValue]
replicateV tag n = [NamedValue (Name $ tag ++ show i) dv | i<-[0..n-1]]


hydroSetup :: Setup Vec3 Int
hydroSetup = Setup $ 
        [NamedValue (Name "density") dv] ++ 
        replicateV "velocity" 3 ++ 
        [NamedValue (Name "pressure") dv]

    
hydroKernelBuilder :: Builder Vec3 Int ()
hydroKernelBuilder = do
  dens <- load Rlm.TLocal (undefined::Double) $ Name "density"
  maxDens <- reduce Reduce.Max $ return dens
  md2 <- broadcast $ 2.718 * return maxDens
  dens2 <- return md2 * return dens - return dens
  store (Name "density") (return dens2)
  return ()

main :: IO ()
main = do
  putStrLn "hi"
  print $ initState hydroSetup
  let 
      (_, s) = runState hydroKernelBuilder $ initState hydroSetup
      g :: Graph Vec3 Int ()
      g = target s
  print $ g
  
  
