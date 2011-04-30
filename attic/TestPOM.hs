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

replicateV :: String -> Int -> [StaticValue]
replicateV tag n = [StaticValue (StaticID $ tag ++ show i) dv | i<-[0..n-1]]

pom :: POM Vec3 Int
pom = POM $ [StaticValue (StaticID "density") dv] ++ 
      replicateV "velocity" 3 ++ 
      [StaticValue (StaticID "pressure") dv]
    
    


main :: IO ()
main = do
  putStrLn "hi"
  print pom

