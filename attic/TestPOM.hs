{-# OPTIONS -Wall #-}

import Data.Typeable
import Language.Paraiso.Tensor
import Language.Paraiso.OM.Builder
import Language.Paraiso.OM.DynValue 
import Language.Paraiso.OM.Graph
import qualified Language.Paraiso.OM.Realm as Rlm
import Language.Paraiso.POM


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

    
main :: IO ()
main = do
  putStrLn "hi"
  print $ initState hydroSetup

