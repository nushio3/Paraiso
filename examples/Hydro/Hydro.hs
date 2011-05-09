{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import qualified Algebra.Additive  as Additive
import qualified Algebra.Field     as Field
import qualified Algebra.Ring      as Ring
import           Data.Typeable
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

bind :: B a -> B (B a)
bind = fmap return
       
loadReal :: Name -> BR
loadReal = load TLocal (undefined::Real) 

----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

delta :: (Eq a, Ring.C b) => a -> a -> b
delta i j = if i==j then Ring.one else Additive.zero

kGamma :: Field.C a => a
kGamma = fromRational' $ 5/3

class Hydrable a where
  density  :: a -> BR
  velocity :: a -> Dim BR
  velocity x = 
    compose (\i -> momentum x !i / density x)
  pressure :: a -> BR
  pressure x = (kGamma-1) * internalEnergy x
  momentum :: a -> Dim BR
  momentum x =
      compose (\i -> density x * velocity x !i)
  energy   :: a -> BR
  energy   x = kineticEnergy x + 1/(kGamma-1) * pressure x
  enthalpy :: a -> BR
  enthalpy x = kineticEnergy x + kGamma/(kGamma-1) * pressure x
  densityFlux  :: a -> Dim BR
  densityFlux  x = momentum x
  momentumFlux :: a -> Dim (Dim BR)
  momentumFlux x = 
      compose (\i -> compose (\j ->
          momentum x !i * velocity x !j + pressure x * delta i j))
  energyFlux   :: a -> Dim BR
  energyFlux x = 
      compose (\i -> enthalpy x * velocity x !i)
  soundSpeed   :: a -> BR
  soundSpeed x = sqrt (kGamma * pressure x / density x)
  kineticEnergy :: a -> BR
  kineticEnergy x = 0.5 * contract (\i -> velocity x !i * momentum x !i)
  internalEnergy :: a -> BR
  internalEnergy x = energy x - kineticEnergy x

data Hydro = Hydro
    {densityH::BR, velocityH::Dim BR, pressureH::BR, 
     momentumH::Dim BR, energyH::BR, enthalpyH::BR,
     densityFluxH::Dim BR, momentumFluxH::Dim (Dim BR), energyFluxH::Dim BR,
     soundSpeedH::BR, kineticEnergyH :: BR, internalEnergyH :: BR}

instance Hydrable Hydro where
  density = densityH; velocity = velocityH; pressure = pressureH;
  momentum = momentumH; energy = energyH; enthalpy = enthalpyH;
  densityFlux = densityFluxH; momentumFlux = momentumFluxH;
  energyFlux = energyFluxH; soundSpeed = soundSpeedH;
  kineticEnergy = kineticEnergyH; internalEnergy = internalEnergyH;



buildProceed :: Builder Dim Int ()
buildProceed = do
  dens <- bind $ loadReal $ Name "density"
  velo <- mapM (bind . loadReal) velocityNames
  pres <- bind $ loadReal $ Name "pressure"
  store (Name "density")  pres

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
 


  
