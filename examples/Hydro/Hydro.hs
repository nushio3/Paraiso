{-# LANGUAGE  DeriveFunctor, DeriveFoldable, DeriveTraversable,
  FlexibleInstances, NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS -Wall #-}

-- | library for hydrodynamic variables.
-- the notations are specified to Builder Vec2 Int 
-- at the moment. and non-hydro-relevant utility functions
-- are defined.
-- but this limitation will be lifted and modules will be separated
-- once everything is working well.


module Hydro(Real, Dim, B, BR, BGR, bind, 
             delta, kGamma, Hydrable(..), Hydro,
             soundSpeed' , 
             bindPrimitive, bindConserved) where

import qualified Algebra.Additive  as Additive
import qualified Algebra.Field     as Field
import qualified Algebra.Ring      as Ring
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Realm 
import           Language.Paraiso.OM.Value as Val
import           Language.Paraiso.Prelude 
import           Language.Paraiso.Tensor

----------------------------------------------------------------
-- Binder monad utilities
----------------------------------------------------------------

type Real = Float
type Dim = Vec2
type B a = Builder Dim Int Annotation a
type BR = B (Value TLocal Real)
type BGR = B (Value TGlobal Real)

bind :: B a -> B (B a)
bind = fmap return
       
----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

delta :: (Eq a, Ring.C b) => a -> a -> b
delta i j = if i==j then Ring.one else Additive.zero

kGamma :: Field.C a => a
kGamma = fromRational' $ 5/3

-- | sound speed as a standalone function.
soundSpeed' :: BR -> BR -> BR
soundSpeed' dens0 pres0 = sqrt (kGamma * pres0 / dens0)

bindPrimitive :: BR -> Dim BR -> BR -> B (Hydro BR)
bindPrimitive density0 velocity0 pressure0 = 
  bindHydro $ PrimitiveVar density0 velocity0 pressure0
  
bindConserved :: BR -> Dim BR -> BR -> B (Hydro BR)
bindConserved density0 momentum0 energy0 = 
  bindHydro $ ConservedVar density0 momentum0 energy0
  

bindHydro :: (Hydrable a) => a -> B (Hydro BR)
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
  enthalpy x = energy x + pressure x
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
  soundSpeed x = soundSpeed' (density x) (pressure x) 
  kineticEnergy :: a -> BR
  kineticEnergy x = 0.5 * contract (\i -> velocity x !i * momentum x !i)
  internalEnergy :: a -> BR
  internalEnergy x = energy x - kineticEnergy x


data Hydro a = Hydro
    {densityHydro::a, velocityHydro::Dim a, pressureHydro::a, 
     momentumHydro::Dim a, energyHydro::a, enthalpyHydro::a,
     densityFluxHydro::Dim a, momentumFluxHydro::Dim (Dim a), 
     energyFluxHydro::Dim a, soundSpeedHydro::a, 
     kineticEnergyHydro::a, internalEnergyHydro::a}
    deriving (Functor, Foldable, Traversable)

instance Hydrable (Hydro BR) where
  density = densityHydro; velocity = velocityHydro; pressure = pressureHydro;
  momentum = momentumHydro; energy = energyHydro; enthalpy = enthalpyHydro;
  densityFlux = densityFluxHydro; momentumFlux = momentumFluxHydro;
  energyFlux = energyFluxHydro; soundSpeed = soundSpeedHydro;
  kineticEnergy = kineticEnergyHydro; internalEnergy = internalEnergyHydro;

instance Applicative Hydro where
  pure x = Hydro
    {densityHydro = x, velocityHydro = pure x, pressureHydro = x, 
     momentumHydro = pure x, energyHydro = x, enthalpyHydro = x,
     densityFluxHydro = pure x, momentumFluxHydro = pure (pure x), 
     energyFluxHydro = pure x, soundSpeedHydro = x, 
     kineticEnergyHydro = x, internalEnergyHydro = x}
  hf <*> hx = Hydro
    {densityHydro        = densityHydro        hf $ densityHydro        hx,
     pressureHydro       = pressureHydro       hf $ pressureHydro       hx,
     energyHydro         = energyHydro         hf $ energyHydro         hx,
     enthalpyHydro       = enthalpyHydro       hf $ enthalpyHydro       hx,
     soundSpeedHydro     = soundSpeedHydro     hf $ soundSpeedHydro     hx,
     kineticEnergyHydro  = kineticEnergyHydro  hf $ kineticEnergyHydro  hx,
     internalEnergyHydro = internalEnergyHydro hf $ internalEnergyHydro hx,
     velocityHydro       = velocityHydro    hf <*> velocityHydro    hx,
     momentumHydro       = momentumHydro    hf <*> momentumHydro    hx,
     densityFluxHydro    = densityFluxHydro hf <*> densityFluxHydro hx,
     energyFluxHydro     = energyFluxHydro  hf <*> energyFluxHydro  hx,
     momentumFluxHydro   = 
         compose(\i -> compose(\j -> (momentumFluxHydro hf!i!j) 
                                     (momentumFluxHydro hx!i!j)))
-- in other words,  
-- (<*>)((<*>) <$> momentumFluxHydro hf) $ momentumFluxHydro hx  
    }                           
    
data PrimitiveVar = PrimitiveVar
    {densityPrim::BR, velocityPrim::Dim BR, pressurePrim::BR}

instance Hydrable PrimitiveVar where
  density = densityPrim; velocity = velocityPrim; pressure = pressurePrim

data ConservedVar = ConservedVar
    {densityCsvd::BR, momentumCsvd::Dim BR, energyCsvd::BR}

instance Hydrable ConservedVar where
  density = densityCsvd; momentum = momentumCsvd; energy = energyCsvd

