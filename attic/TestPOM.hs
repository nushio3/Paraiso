{-# OPTIONS -Wall #-}

import Language.Paraiso.Tensor
import Language.Paraiso.POM
import Language.Paraiso.POM.Builder

replicateID :: String -> Int -> [StaticID]
replicateID tag n = [StaticID $ tag ++ show i| i<-[0..n-1]]

pom :: POM Vec3 Int
pom = POM $ [StaticID "density"] ++ replicateID "velocity" 3 ++ [StaticID "pressure"]
    
    


main :: IO ()
main = do
  putStrLn "hi"
  print pom

