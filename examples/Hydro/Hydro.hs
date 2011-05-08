{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import           Data.Typeable
--import qualified Data.Graph.Inductive as FGL
import           Language.Paraiso.Generator.Cpp
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.OM.Value as Val
import           Language.Paraiso.POM
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor
import           System.Directory (createDirectoryIfMissing)

type Real = Double
type Dim = Vec2
type B a = Builder Dim Int a
type BR = B (Value TLocal Real)

realDV :: DynValue
realDV = DynValue{DVal.realm = Local, DVal.typeRep = typeOf (0::Real)}

intGDV :: DynValue
intGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Int)}

realGDV :: DynValue
realGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Real)}


-- the list of static variables for this machine
pomSetup :: Setup Dim Int 
pomSetup = Setup $ 
            [Named (Name "generation") intGDV] ++
            [Named (Name "time") realGDV] ++
            foldMap (\name0 -> [Named name0 realGDV]) dRNames ++ 
            foldMap (\name0 -> [Named name0 realGDV]) extentNames ++ 
            [Named (Name "density") realDV]  ++
            foldMap (\name0 -> [Named name0 realDV]) velocityNames ++ 
            [Named (Name "pressure") realDV]  

velocityNames :: Dim (Name)
velocityNames = compose (\axis -> Name $ "velocity" ++ show (axisIndex axis))

dRNames :: Dim (Name)
dRNames = compose (\axis -> Name $ "dR" ++ show (axisIndex axis))

extentNames :: Dim (Name)
extentNames = compose (\axis -> Name $ "extent" ++ show (axisIndex axis))

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
       
loadBindReal :: Name -> B (BR)
loadBindReal = bind . load TLocal (undefined::Real) 

----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

soundSpeed :: BR -> BR -> B (BR)
soundSpeed density pressure = bind $ sqrt (pressure / density)

buildProceed :: Builder Dim Int ()
buildProceed = do
  density <- loadBindReal $ Name "density"
  velocity <- mapM loadBindReal velocityNames
  pressure <- loadBindReal $ Name "pressure"
  sound <- soundSpeed density pressure
  store (Name "density")  sound

buildInit :: Builder Dim Int ()
buildInit = do
  return ()
  
-- compose the machine.
pom :: POM Dim Int (Strategy Cpp)
pom = fmap (\() -> autoStrategy) $ 
  makePOM (Name "Hydro")  pomSetup
    [(Name "init"   , buildInit),
     (Name "proceed", buildProceed)]
              

main :: IO ()
main = do
  createDirectoryIfMissing True "output"
  writeFile "output/POM.txt" $ show pom ++ "\n"
  writeFile "output/POM1.txt" $ show (decideStrategy pom) ++ "\n"
  generate Cpp pom "dist"
 


  
