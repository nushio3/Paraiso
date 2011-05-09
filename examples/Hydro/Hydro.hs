{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS -Wall #-}

-- | library for hydrodynamic variables.
-- the notations are specified to Builder Vec2 Int 
-- at the moment. and non-hydro-relevant utility functions
-- are defined.
-- but this limitation will be lifted and modules will be separated
-- once everything is working well.


module Hydro(Real, Dim, B, BR, bind,
             delta, kGamma, Hydrable(..),
             bindPrimitive, bindConserved) where

import qualified Algebra.Additive  as Additive
import qualified Algebra.Field     as Field
import qualified Algebra.Ring      as Ring
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Realm 
import           Language.Paraiso.OM.Value as Val
import           Language.Paraiso.Prelude 
import           Language.Paraiso.Tensor

----------------------------------------------------------------
-- Binder monad utilities
----------------------------------------------------------------

type Real = Double
type Dim = Vec2
type B a = Builder Dim Int a
type BR = B (Value TLocal Real)

bind :: B a -> B (B a)
bind = fmap return
       

----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

delta :: (Eq a, Ring.C b) => a -> a -> b
delta i j = if i==j then Ring.one else Additive.zero

kGamma :: Field.C a => a
kGamma = fromRational' $ 5/3

bindPrimitive :: BR -> Dim BR -> BR -> B Hydro
bindPrimitive density0 velocity0 pressure0 = 
  bindHydro $ PrimitiveVar density0 velocity0 pressure0
  
bindConserved :: BR -> Dim BR -> BR -> B Hydro
bindConserved density0 momentum0 energy0 = 
  bindHydro $ ConservedVar density0 momentum0 energy0
  

bindHydro :: (Hydrable a) => a -> B Hydro
bindHydro x = do
  densityBound <- bind $ density x
  velocityBound <- mapM bind $ velocity x
  pressureBound <- bind $ pressure x
  momentumBound <- mapM bind $ momentum x
  energyBound <- bind $ energy x
  enthalpyBound <- bind $ enthalpy x
  densityFluxBound <- mapM bind $ densityFlux x
  momentumFluxBound <-  mapM (mapM bind) $ momentumFlux x
  energyFluxBound <- mapM bind $ energyFlux x
  soundSpeedBound <- bind $ soundSpeed x
  kineticEnergyBound <- bind $ kineticEnergy x
  internalEnergyBound <- bind $ internalEnergy x
  return Hydro{
    densityHydro = densityBound,
    velocityHydro = velocityBound,
    pressureHydro = pressureBound,
    momentumHydro = momentumBound,
    energyHydro = energyBound,
    enthalpyHydro = enthalpyBound,
    densityFluxHydro = densityFluxBound,
    momentumFluxHydro = momentumFluxBound,
    energyFluxHydro = energyFluxBound,
    soundSpeedHydro = soundSpeedBound,
    kineticEnergyHydro = kineticEnergyBound,
    internalEnergyHydro = internalEnergyBound
             }
  

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
    {densityHydro::BR, velocityHydro::Dim BR, pressureHydro::BR, 
     momentumHydro::Dim BR, energyHydro::BR, enthalpyHydro::BR,
     densityFluxHydro::Dim BR, momentumFluxHydro::Dim (Dim BR), 
     energyFluxHydro::Dim BR, soundSpeedHydro::BR, 
     kineticEnergyHydro:: BR, internalEnergyHydro:: BR}

instance Hydrable Hydro where
  density = densityHydro; velocity = velocityHydro; pressure = pressureHydro;
  momentum = momentumHydro; energy = energyHydro; enthalpy = enthalpyHydro;
  densityFlux = densityFluxHydro; momentumFlux = momentumFluxHydro;
  energyFlux = energyFluxHydro; soundSpeed = soundSpeedHydro;
  kineticEnergy = kineticEnergyHydro; internalEnergy = internalEnergyHydro;

data PrimitiveVar = PrimitiveVar
    {densityPrim::BR, velocityPrim::Dim BR, pressurePrim::BR}

instance Hydrable PrimitiveVar where
  density = densityPrim; velocity = velocityPrim; pressure = pressurePrim

data ConservedVar = ConservedVar
    {densityCsvd::BR, velocityCsvd::Dim BR, pressureCsvd::BR}

instance Hydrable ConservedVar where
  density = densityCsvd; velocity = velocityCsvd; pressure = pressureCsvd

