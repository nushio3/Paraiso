{-# OPTIONS -Wall #-}

import Language.Paraiso.Tensor
import Language.Paraiso.POM

pom :: POM Vec3 Int
pom = POM $ [StaticID "density"]
    
    



main = do
  putStrLn "hi"
  print pom

