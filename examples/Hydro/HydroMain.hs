{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import           Data.Typeable
import           Language.Paraiso.Generator.Cpp
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.POM
import           Language.Paraiso.Prelude 
import           Language.Paraiso.Tensor
import           Hydro
import           System.Directory (createDirectoryIfMissing)

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

loadReal :: Name -> BR
loadReal = load TLocal (undefined::Real) 
loadGReal :: Name -> BGR
loadGReal = load TGlobal (undefined::Real) 

----------------------------------------------------------------
-- Hydro utility functions.
----------------------------------------------------------------

hllc :: Axis Dim -> Hydro BR -> Hydro BR -> B (Hydro BR)
hllc i left right = do
  densMid  <- bind $ (density left    + density right   ) / 2
  soundMid <- bind $ (soundSpeed left + soundSpeed right) / 2
  let 
      speedLeft  = velocity left  !i
      speedRight = velocity right !i
  presStar <- bind $ max 0 $ (pressure left   + pressure right  ) / 2 -
              densMid * soundMid * (speedRight - speedLeft) 
  shockLeft <- bind $ velocity left !i - 
               soundSpeed left * hllcQ presStar (pressure left)
  shockRight <- bind $ velocity right !i +
               soundSpeed right * hllcQ presStar (pressure right)
  shockStar <- bind $ (pressure right - pressure left
		       + density left  * speedLeft  * (shockLeft  - speedLeft)
		       - density right * speedRight * (shockRight - speedRight) )
               / (density left  * (shockLeft  - speedLeft ) - 
                  density right * (shockRight - speedRight) )
  lesta <- starState shockStar shockLeft  left
  rista <- starState shockStar shockRight right
  let selector a b c d =
          select (0 `lt` shockLeft) a $ 
          select (0 `lt` shockStar) b $
          select (0 `lt` shockRight) c d
  mapM bind $ selector <$> left <*> lesta <*> rista <*> right
    where
      hllcQ sp p = select (p `le` sp) 1 $
                   sqrt(1 + (kGamma + 1)/(2*kGamma) * (sp / p - 1))
      starState starShock shock x = do
        let speed = velocity x !i
        dens <- bind $ density x * (shock - speed) / (shock - starShock)
        mome <- sequence $ compose 
                (\j ->  bind $ dens * if i==j then starShock
                                      else velocity x !j)
        enrg <- bind $ dens * 
                (energy x / density x + 
                 (starShock - speed) * 
                 (starShock + pressure x/density x/(shock - speed)))
        bindConserved dens mome enrg

buildProceed :: Builder Dim Int ()
buildProceed = do
  dens    <- bind $ loadReal $ Name "density"
  velo    <- mapM (bind . loadReal) velocityNames
  pres    <- bind $ loadReal $ Name "pressure"
  
  timeG   <- bind $ loadGReal $ Name "time"
  
  dRG     <- mapM (bind . loadGReal) dRNames  
  dR      <- mapM (bind . broadcast) dRG 
  cell <-  bindPrimitive dens velo pres
  
  let timescale i = dR!i / (soundSpeed cell + abs (velocity cell !i))
  dts <- bind $ foldl1 min $ compose timescale
  
  dtG <- bind $ reduce Reduce.Min dts
  dt  <- bind $ broadcast dtG
  
  cell2 <- proceedSingle 1 (dt/2) dR cell  cell
  cell3 <- proceedSingle 2  dt    dR cell2 cell
  
  store (Name "time") $ timeG + dtG
  store (Name "density") $ density cell3
  _ <- sequence $ compose(\i ->  store (velocityNames!i) $ velocity cell3 !i)
  store (Name "pressure") $ pressure cell3


proceedSingle :: Int -> BR -> Dim BR -> Hydro BR -> Hydro BR -> B (Hydro BR)
proceedSingle order dt dR cellF cellS = do
  let calcWall i = do
        (lp,rp) <- interpolate order i cellF
        hllc i lp rp
  wall <- sequence $ compose calcWall
  cx <- addFlux dt dR wall (Axis 0) cellS
  addFlux dt dR wall (Axis 1) cx
  
addFlux :: BR -> Dim BR -> Dim (Hydro BR) -> Axis Dim -> Hydro BR -> B (Hydro BR)
addFlux dt dR wall ex cell = do
  dtdx <- bind $ dt / dR!ex
  leftWall  <- mapM bind $ wall ! ex
  rightWall <- mapM (bind . shift (negate $ unitVector ex)) $ wall!ex
  dens1 <- bind $ density cell + dtdx * (densityFlux leftWall - densityFlux rightWall)!ex
  mome1 <- sequence $ compose 
           (\j -> bind $ (momentum cell !j + dtdx * 
                          (momentumFlux leftWall - momentumFlux rightWall) !j!ex))
  enrg1 <- bind $ energy  cell + dtdx * (energyFlux leftWall - energyFlux rightWall)  !ex
  
  bindConserved dens1 mome1 enrg1

interpolateSingle :: Int -> BR -> BR -> BR -> BR -> B (BR,BR)
interpolateSingle order x0 x1 x2 x3 = 
  if order == 1 
  then do
    return (x1, x2)
  else if order == 2
       then do
         d01 <- bind $ x1-x0
         d12 <- bind $ x2-x1
         d23 <- bind $ x3-x2
         let absmaller a b = select ((a*b) `le` 0) 0 $ select (abs a `lt` abs b) a b
         d1 <- bind $ absmaller d01 d12
         d2 <- bind $ absmaller d12 d23
         l <- bind $ x1 + d1/2
         r <- bind $ x2 - d2/2
         return (l, r)
       else error $ show order ++ "th order spatial interpolation is not yet implemented"

interpolate :: Int -> Axis Dim -> Hydro BR -> B (Hydro BR, Hydro BR)
interpolate order i cell = do
  let shifti n =  shift $ compose (\j -> if i==j then n else 0)
  a0 <- mapM (bind . shifti ( 2)) cell
  a1 <- mapM (bind . shifti ( 1)) cell
  a2 <- mapM (bind . shifti ( 0)) cell
  a3 <- mapM (bind . shifti (-1)) cell
  intp <- sequence $ interpolateSingle order <$> a0 <*> a1 <*> a2 <*> a3
  let (l,r) = (fmap fst intp , fmap snd intp)
      bp :: Hydro BR -> B (Hydro BR)
      bp x = do 
        dens1 <- bind $ density x
        velo1 <- mapM bind $ velocity x
        pres1 <- bind $ pressure x
        bindPrimitive dens1 velo1 pres1
  lp <- bp l
  rp <- bp r
  return (lp,rp)

  
  
  

buildInit2 :: Builder Dim Int ()
buildInit2 = do
  dRG     <- mapM (bind . loadGReal) dRNames
  extentG <- mapM (bind . loadGReal) extentNames
  dR <- mapM (bind . broadcast) dRG 
  extent <- mapM (bind . broadcast) extentG
  icoord  <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Double) axis)  
  coord   <- mapM bind $ compose (\i -> dR!i * icoord!i)

  let ey = Axis 1
      vplus, vminus :: Dim BR
      vplus  = Vec :~ ( 0.3) :~ 0
      vminus = Vec :~ (-0.3) :~ 0
  
  region <- bind $ (coord!ey) `lt` (0.5*extent!ey)
  velo <- sequence $ compose (\i -> bind $ select region (vplus!i) (vminus!i))

  factor <- bind $ select region 1 2

  store (Name "density") $ factor * kGamma * (kGamma::BR)
  _ <- sequence $ compose(\i -> store (velocityNames!i) $ velo !i)
  store (Name "pressure") $ factor * (kGamma::BR)


buildInit1 :: Builder Dim Int ()
buildInit1 = do
  dRG     <- mapM (bind . loadGReal) dRNames
  extentG <- mapM (bind . loadGReal) extentNames
  dR <- mapM (bind . broadcast) dRG 
  extent <- mapM (bind . broadcast) extentG
  icoord  <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Double) axis)  
  coord   <- mapM bind $ compose (\i -> dR!i * icoord!i)

  let ex = Axis 0
  
  region <- bind $ (coord!ex) `lt` (0.5*extent!ex)
  
  dens <- bind $ select region (1.0::BR) (0.125)
  velo <- sequence $ compose (\_ -> bind $ (0::BR))
  pres <- bind $ select region (1.0::BR) (0.1)

  store (Name "density") $ dens
  _ <- sequence $ compose(\i -> store (velocityNames!i) $ velo !i)
  store (Name "pressure") $ pres


  
-- compose the machine.
pom :: POM Dim Int (Strategy Cpp)
pom = fmap (\() -> autoStrategy) $ 
  makePOM (Name "Hydro")  pomSetup
    [(Name "init_shocktube"   , buildInit1),
     (Name "init_kh"   , buildInit2),
     (Name "proceed", buildProceed)]
              

main :: IO ()
main = do
  createDirectoryIfMissing True "output"
--  writeFile "output/POM.txt" $ show pom ++ "\n"
--  writeFile "output/POM1.txt" $ show (decideStrategy pom) ++ "\n"
  generate Cpp pom "dist"
 


  
