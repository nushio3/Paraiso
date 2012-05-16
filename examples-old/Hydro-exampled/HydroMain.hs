{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import qualified Data.Text.IO as T
import           Data.Typeable
import           Hydro
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import           Language.Paraiso.Name
import           Language.Paraiso.Generator (generateIO)
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Builder
import           Language.Paraiso.OM.Builder.Boolean
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.PrettyPrint
import           Language.Paraiso.OM.Realm 
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.Optimization
import           Language.Paraiso.Prelude 
import           Language.Paraiso.Tensor
import           System.Directory (createDirectoryIfMissing)

realDV :: DynValue
realDV = DynValue{DVal.realm = Local, DVal.typeRep = typeOf (0::Real)}

intGDV :: DynValue
intGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Int)}

realGDV :: DynValue
realGDV = DynValue{DVal.realm = Global, DVal.typeRep = typeOf (0::Real)}


-- the list of static variables for this machine
hydroVars :: [Named DynValue]
hydroVars = 
  [Named (mkName "generation") intGDV] ++
  [Named (mkName "time") realGDV] ++
  [Named (mkName "cfl") realGDV] ++
  foldMap (\name0 -> [Named name0 realGDV]) dRNames ++ 
  foldMap (\name0 -> [Named name0 realGDV]) extentNames ++ 
  [Named (mkName "density") realDV]  ++
  foldMap (\name0 -> [Named name0 realDV]) velocityNames ++ 
  [Named (mkName "pressure") realDV]  

velocityNames :: Dim (Name)
velocityNames = compose (\axis -> mkName $ "velocity" ++ showT (axisIndex axis))

dRNames :: Dim (Name)
dRNames = compose (\axis -> mkName $ "dR" ++ showT (axisIndex axis))

extentNames :: Dim (Name)
extentNames = compose (\axis -> mkName $ "extent" ++ showT (axisIndex axis))

loadReal :: Name -> BR
loadReal = load TLocal (undefined::Real) 
loadGReal :: Name -> BGR
loadGReal = load TGlobal (undefined::Real) 

----------------------------------------------------------------
-- Hydro Implementations
----------------------------------------------------------------

buildInit :: B ()
buildInit = do
  dRG     <- mapM (bind . loadGReal) dRNames
  extentG <- mapM (bind . loadGReal) extentNames
  dR <- mapM (bind . broadcast) dRG 
  extent <- mapM (bind . broadcast) extentG
  icoord  <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Real) axis)  
  coord   <- mapM bind $ compose (\i -> dR!i * icoord!i)

  let ex = Axis 0
      ey = Axis 1
      vplus, vminus :: Dim BR
      vplus  = Vec :~ ( 6) :~ 0
      vminus = Vec :~ ( 0) :~ 0

  region <- bind $ (coord!ey) `gt` (0.47*extent!ey) && (coord!ey) `lt` (0.53*extent!ey) && (coord!ex) `lt` 0

  velo <- sequence $ compose (\i -> bind $ select region (vplus!i) (vminus!i))

  factor <- bind $ 1 + 1e-3 * sin (6 * pi * coord ! ex)

  store (mkName "density") $ factor * kGamma * (kGamma::BR) * (select region 1 10)
  _ <- sequence $ compose(\i -> store (velocityNames!i) $ velo !i)
  store (mkName "pressure") $ factor * 0.6


boundaryCondition :: Hydro BR -> B (Hydro BR)
boundaryCondition cell = do
  dRG     <- mapM (bind . loadGReal) dRNames
  extentG <- mapM (bind . loadGReal) extentNames
  dR <- mapM (bind . broadcast) dRG 
  extent <- mapM (bind . broadcast) extentG
  icoord  <- sequenceA $ compose (\axis -> bind $ loadIndex (0::Real) axis)  
  isize   <- sequenceA $ compose (\axis -> bind $ loadSize  TLocal (0::Real) axis)        
  coord   <- mapM bind $ compose (\i -> dR!i * icoord!i)
  
  let ex = Axis 0
      ey = Axis 1
      vplus, vminus :: Dim BR
      vplus  = Vec :~ ( 6) :~ 0
      vminus = Vec :~ ( 0) :~ 0

  region <- bind $ (coord!ey) `gt` (0.47*extent!ey) && (coord!ey) `lt` (0.53*extent!ey) && (coord!ex) `lt` 0

  cell0 <- bindPrimitive 
           (kGamma * (kGamma::BR) * (select region 1 10))
           (compose (\i -> select region (vplus!i) (vminus!i)))
           (0.6)

  outOf <- bind $ 
       (foldl1 (||) $ compose (\i -> icoord !i `lt` 0)) ||
       (foldl1 (||) $ compose (\i -> (icoord !i) `ge` (isize !i)))
  return $ (\a b -> Anot.add Alloc.Manifest <?> (select outOf a b)) <$> cell0 <*> cell

buildProceed :: B ()
buildProceed = do
  dens    <- bind $ loadReal $ mkName "density"
  velo    <- mapM (bind . loadReal) velocityNames
  pres    <- bind $ loadReal $ mkName "pressure"

  timeG   <- bind $ loadGReal $ mkName "time"
  cflG    <- bind $ loadGReal $ mkName "cfl"

  dRG     <- mapM (bind . loadGReal) dRNames  
  dR      <- mapM (bind . broadcast) dRG 
  cell0 <-  bindPrimitive dens velo pres

  cell <- boundaryCondition cell0


  let timescale i = dR!i / (soundSpeed cell + abs (velocity cell !i))
  dts <- bind $ foldl1 min $ compose timescale

  dtG <- bind $ cflG * reduce Reduce.Min dts
  dt  <- bind $ broadcast dtG

  cell2 <- proceedSingle 1 (dt/2) dR cell  cell
  cell3 <- proceedSingle 2  dt    dR cell2 cell

  store (mkName "time") $ timeG + dtG
  store (mkName "density") $ density cell3
  _ <- sequence $ compose(\i ->  store (velocityNames!i) $ velocity cell3 !i)
    
  
  store (mkName "pressure") $ pressure cell3




proceedSingle :: Int -> BR -> Dim BR -> Hydro BR -> Hydro BR -> B (Hydro BR)
proceedSingle order dt dR cellF cellS = do
  let calcWall i = do
        (lp,rp) <- interpolate order i cellF
        hllc i lp rp
  wall <- sequence $ compose calcWall
  cellN <- foldl1 (.) (compose (\i -> (>>= addFlux dt dR wall i))) $ return cellS

  bindPrimitive 
    (max 1e-2 $ density cellN)
    (velocity cellN)
    (max 1e-2 $ pressure cellN)

--  cx <- addFlux dt dR wall (Axis 0) cellS
--  addFlux dt dR wall (Axis 1) cx

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





-- compose the machine.
myOM :: OM Dim Int Anot.Annotation
myOM =  optimize O3 $ 
  makeOM (mkName "Hydro") [] hydroVars
    [(mkName "init"   , buildInit),
     (mkName "proceed", buildProceed)]


cpuSetup :: Native.Setup Vec2 Int
cpuSetup = 
  (Native.defaultSetup $ Vec :~ 1024 :~ 1024)
  { Native.directory = "./dist/" 
  }

gpuSetup :: Native.Setup Vec2 Int
gpuSetup = 
  (Native.defaultSetup $ Vec :~ 1024 :~ 1024)
  { Native.directory = "./dist-cuda/" ,
    Native.language  = Native.CUDA
  }



main :: IO ()
main = do
  createDirectoryIfMissing True "output"
  -- output the intermediate state.
  T.writeFile "output/OM.txt" $ prettyPrintA1 $ myOM

  -- generate the cpu library 
  _ <- generateIO cpuSetup myOM

  -- generate the gpu library 
  _ <- generateIO gpuSetup myOM

  return ()



  
