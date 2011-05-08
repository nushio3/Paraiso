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

type Dim = Vec2
type B a = Builder Dim Int a
type BD = B (Value TLocal Double)

doubleDV :: DynValue
doubleDV = DynValue{DVal.realm = Local, DVal.typeRep = typeOf (0::Double)}

intGDV :: DynValue
intGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Int)}

doubleGDV :: DynValue
doubleGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Double)}


-- the list of static variables for this machine
pomSetup :: Setup Dim Int 
pomSetup = Setup $ 
            [Named (Name "generation") intGDV] ++
            [Named (Name "time") doubleGDV] ++
            [Named (Name "density") doubleDV]  ++
            foldMap (\name0 -> [Named name0 doubleDV]) velocityNames ++ 
            [Named (Name "pressure") doubleDV]  

velocityNames :: Dim (Name)
velocityNames = compose (\axis -> Name $ "velocity" ++ show (axisIndex axis))

bind :: (Functor f, Monad m) => f a -> f (m a)
bind = fmap return
       
loadBindDouble :: Name -> B (BD)
loadBindDouble = bind . load TLocal (undefined::Double) 

----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

soundSpeed :: BD -> BD -> B (BD)
soundSpeed density pressure = bind $ sqrt (pressure / density)

buildProceed :: Builder Dim Int ()
buildProceed = do
  density <- loadBindDouble $ Name "density"
  velocity <- mapM loadBindDouble velocityNames
  pressure <- loadBindDouble $ Name "pressure"
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
 


  
