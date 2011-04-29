{-# OPTIONS -Wall #-}

import Data.Typeable
import Language.Paraiso.Tensor
import Language.Paraiso.POM
import Language.Paraiso.POM.Graph
import Language.Paraiso.POM.Value as V
import Language.Paraiso.POM.Builder

tp :: TypeRep
tp = typeOf (0::Double)

dv :: DynValue
dv = DynValue{rea = Local, typeRep = tp}

replicateID :: String -> Int -> [StaticValue]
replicateID tag n = [StaticValue (tag ++ show i) dv | i<-[0..n-1]]

pom :: POM Vec3 Int
pom = POM $ [StaticValue "density" dv] ++ 
      replicateID "velocity" 3 ++ 
      [StaticValue "pressure" dv]
    
    


main :: IO ()
main = do
  putStrLn "hi"
  print pom

