{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, NoImplicitPrelude, PackageImports,
 FlexibleContexts,  StandaloneDeriving, UndecidableInstances  #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Tuning.Genetic
  (
   Genome, Species(..),
   makeSpecies,
   readGenome, overwriteGenome,
   mutate, cross, triangulate,
   generateIO
  ) where

import qualified "mtl" Control.Monad.State as State
import qualified Data.Graph.Inductive                   as FGL
import qualified Data.Vector as V
import           Data.Vector ((!))
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import qualified Language.Paraiso.Annotation.SyncThreads as Sync
import qualified Language.Paraiso.Generator.Native as Native
import qualified Language.Paraiso.OM as OM
import qualified Language.Paraiso.OM.Graph as OM
import qualified Language.Paraiso.Optimization          as Opt
import           Language.Paraiso.Prelude hiding (Boolean(..))
import qualified Language.Paraiso.Generator as Gen  (generateIO) 
import qualified Prelude as Prelude
import           NumericPrelude hiding ((++))
import           System.Random 
import qualified Text.Read as Read

data Species v g = 
  Species {
    setup   :: Native.Setup v g,
    machine :: OM.OM v g Anot.Annotation
  } 
deriving instance (Show (Native.Setup v g),  Show (OM.OM v g Anot.Annotation))
         => Show (Species v g)

makeSpecies :: Native.Setup v g -> OM.OM v g Anot.Annotation -> Species v g
makeSpecies = Species

generateIO :: (Opt.Ready v g) => Species v g -> IO [(FilePath, Text)]
generateIO (Species s om) = Gen.generateIO s om

newtype Genome = Genome [Bool] deriving (Eq)

instance Show Genome where
  show (Genome xs) = show $ toDNA xs

instance Read Genome where
  readPrec = fmap (Genome . fromDNA) Read.readPrec

toDNA :: [Bool] -> String
toDNA xss@(x:xs)
  | even $ length xss = 'C' : inner xss
  | not x             = 'A' : inner xs
  | otherwise         = 'T' : inner xs
  where
    inner [] = ""
    inner (x:y:xs) = inner1 x y : inner xs
    inner _ = error "parity conservation law is broken"

    inner1 False False = 'A'
    inner1 False True  = 'C'
    inner1 True  False = 'G'
    inner1 True  True  = 'T'

fromDNA :: String -> [Bool]
fromDNA xss@(x:xs) 
  | x == 'C'  = inner xs
  | x == 'A'  = False : inner xs
  | x == 'T'  = True  : inner xs
  | otherwise = error "bad DNA"
  where
    inner = concat . map inner1
    inner1 :: Char -> [Bool]
    inner1 'A' = [False, False]
    inner1 'C' = [False, True ]
    inner1 'G' = [True , False]
    inner1 'T' = [True , True ]
    inner1 _   = error "bad DNA"

mutate :: Genome -> IO Genome
mutate original@(Genome xs) = do
  let oldVector = V.fromList xs
      n = length xs
      logN :: Double
      logN = log (fromIntegral n)
  -- 20% of the mutations are single point mutation
  logRand <- randomRIO (-0.2 * logN, logN)
  mutaCoin <- randomRIO (0, 1::Double)
  let randN :: Int
      randN = Prelude.max 1 $ ceiling $ exp logRand
      randRanges = V.replicate randN (0, n - 1)
      randUpd range = do
        idx <- randomRIO range
        let newVal
              -- 50% are negating mutations
              | mutaCoin < 0.5 || randN <= 1 = not $ oldVector ! idx
              -- 25% are all True mutation
              | mutaCoin < 0.75              = True
              -- other 25% are all False mutation
              | otherwise                    = False 
        return (idx, newVal)
  randUpds <- V.mapM randUpd randRanges
  let pureMutant = Genome $ V.toList $ V.update oldVector randUpds
  if randN > 8
     then return pureMutant >>= cross original >>= cross original
     else return pureMutant

cross :: Genome -> Genome -> IO Genome
cross (Genome xs0) (Genome ys0) = do
  swapCoin <- randomRIO (0,1)
  let 
    (xs,ys) = if swapCoin < (0.5::Double) then (xs0, ys0) else (ys0, xs0)
    n =  Prelude.max (length xs) (length ys)
    vx = V.fromList $ take n $ xs ++ repeat False
    vy = V.fromList $ take n $ ys ++ repeat False
    atLeast :: Int -> IO Int
    atLeast n = do
      coin <- randomRIO (0,1)
      if coin < (0.5 :: Double) 
         then return n
         else atLeast (n+1)
  randN <- atLeast 1
  let randRanges = replicate randN (-1, n + 1)
  crossPoints <- mapM randomRIO randRanges
  let vz = V.generate n $ \i ->
           if odd $ length $ filter (<i) crossPoints then vx!i else vy!i
  return $ Genome $ V.toList $ vz

triangulate :: Genome -> Genome -> Genome -> IO Genome
triangulate (Genome base) (Genome left) (Genome right) = do
  return $ Genome $ zipWith3 f base left right
    where
      f b l r = if b/=l then l else r

readGenome :: Species v g -> Genome
readGenome spec =
  encode $ do
    let (x,y) = Native.cudaGridSize $ setup spec
    putInt 16 x
    putInt 16 y
    let om    = machine spec
        kerns = OM.kernels om
    V.mapM_ (putGraph . OM.dataflow) kerns

overwriteGenome :: (Opt.Ready v g) => Genome -> Species v g -> Species v g
overwriteGenome dna oldSpec = 
  decode dna $ do
    -- load cuda grid topology from the genome
    x <- getInt 16
    y <- getInt 16
    let oldSetup = setup oldSpec
        oldOM = machine oldSpec
        oldKernels = OM.kernels oldOM
        oldFlags = OM.globalAnnotation $ OM.setup $ oldOM
    let overwriteKernel kern = do
                 let graph = OM.dataflow kern
                 newGraph <- overwriteGraph graph
                 return $ kern{OM.dataflow = newGraph}
    -- load manifesto from the genome
    newKernels <- V.mapM overwriteKernel oldKernels
    let newGrid = (x,y)
        newSetup = oldSetup {Native.cudaGridSize = newGrid}
        -- reset the optimization flag and cause the optimization again
        newFlags = Anot.set Opt.Unoptimized oldFlags
        newOM = oldOM 
          { OM.kernels = newKernels, 
            OM.setup   = (OM.setup oldOM){ OM.globalAnnotation = newFlags }
          }
    return $ Species newSetup (Opt.optimize Opt.O3 newOM)



newtype Get a = Get { getGet :: State.State [Bool] a }
    deriving (Prelude.Functor, Prelude.Applicative, Monad)
newtype Put a = Put { getPut :: State.State [Bool] a }
    deriving (Prelude.Functor, Prelude.Applicative, Monad)

get :: Get Bool
get = Get $ do
  dat <- State.get
  case dat of
    (x:xs) -> do
      State.put xs
      return x
    _ -> return False -- When the data is depleted default to False

put :: Bool -> Put ()
put x = Put $ do
  xs <- State.get
  State.put $ x:xs

decode :: Genome -> Get a -> a
decode (Genome dna) m = State.evalState (getGet m) dna

encode :: Put a -> Genome 
encode m = Genome $ reverse $ State.execState (getPut m) []

putInt :: Int -> Int -> Put ()
putInt bit n 
  | bit <= 0  = return ()
  | n >= val  = do
    put True
    putInt (bit-1) (n-val)
  | otherwise = do
    put False
    putInt (bit-1) n
  where 
    val :: Int
    val = 2^(fromIntegral $ bit-1)

getInt :: Int -> Get Int
getInt bit
  | bit <= 0  = return 0
  | otherwise = do
    x <- get
    y <- getInt (bit-1)
    return $ y + 2^(fromIntegral $ bit-1) * (if x then 1 else 0)

putGraph :: OM.Graph v g Anot.Annotation -> Put ()
putGraph graph = do
  V.mapM_ put focus
  V.mapM_ put2 focus2
  where
    focus = 
      V.map (isManifest . OM.getA . snd) $
      V.filter (hasChoice . OM.getA . snd) idxNodes

    -- idxNodes :: V.Vector (FGL.Node, OM.Node v g a)
    idxNodes = V.fromList $ FGL.labNodes graph

    hasChoice :: Anot.Annotation -> Bool
    hasChoice anot = 
      case Anot.toMaybe anot of
        Just (Alloc.AllocationChoice _) -> True
        _                               -> False

    isManifest :: Anot.Annotation -> Bool
    isManifest anot = 
      case Anot.toMaybe anot of
        Just Alloc.Manifest -> True
        _                   -> False

    focus2 = 
      V.map (getSyncBools . OM.getA . snd) $
      V.filter (isValue . snd) idxNodes

    isValue nd = case nd of
      OM.NValue _ _ -> True
      _             -> False

    getSyncBools :: Anot.Annotation -> (Bool, Bool)
    getSyncBools xs = let ys = Anot.toList xs in
      (Sync.Pre `elem` ys, Sync.Post `elem` ys)

    put2 (a,b) = put a >> put b



overwriteGraph :: OM.Graph v g Anot.Annotation -> Get (OM.Graph v g Anot.Annotation)
overwriteGraph graph = do
  ovs  <- V.mapM getAt  focusIndices
  ovs2 <- V.mapM getAt2 focus2Indices  
  return $ overwrite ovs2 $ overwrite ovs graph
  where
    overwrite ovs = 
      let updater :: V.Vector (Anot.Annotation -> Anot.Annotation)
          updater = 
            flip V.update ovs $
            V.map (const id) idxNodes in
      OM.imap $ \idx anot -> updater ! idx $ anot

    getAt idx = do
      ret <- get
      return (idx, Anot.set $ if ret then Alloc.Manifest else Alloc.Delayed)

    getAt2 idx = do
      a <- get
      b <- get
      return (idx, (if a then Anot.set Sync.Pre else id) . (if b then Anot.set Sync.Post else id))

    focusIndices = 
      V.map fst $
      V.filter (hasChoice . OM.getA . snd) idxNodes

    focus2Indices = 
      V.map fst $
      V.filter (isValue . snd) idxNodes

    idxNodes = V.fromList $ FGL.labNodes graph

    hasChoice :: Anot.Annotation -> Bool
    hasChoice anot = 
      case Anot.toMaybe anot of
        Just (Alloc.AllocationChoice _) -> True
        _                               -> False

    isManifest :: Anot.Annotation -> Bool
    isManifest anot = 
      case Anot.toMaybe anot of
        Just Alloc.Manifest -> True
        _                   -> False

    isValue nd = case nd of
      OM.NValue _ _ -> True
      _             -> False



{-
overwriteGraph :: OM.Graph v g Anot.Annotation -> Get (OM.Graph v g Anot.Annotation)
overwriteGraph graph = do
  ovs <- V.mapM getAt focusIndices
  return $ overwritten ovs
  where
    overwritten ovs = 
      let newManifest ::  V.Vector (Maybe Alloc.Allocation)
          newManifest = 
            flip V.update ovs $
            V.map (const Nothing) anots in
      flip OM.imap graph $ \idx anot -> 
        case newManifest ! idx of
          Nothing -> anot
          Just x  -> Anot.set x anot

    getAt idx = do
      ret <- get
      return (idx, Just $ if ret then Alloc.Manifest else Alloc.Delayed)

    focusIndices = 
      V.map fst $
      V.filter (hasChoice . snd) anots

    anots :: V.Vector (FGL.Node, Anot.Annotation)
    anots = V.fromList $ map (\(n, lab) -> (n, OM.getA lab)) $ FGL.labNodes graph


    hasChoice :: Anot.Annotation -> Bool
    hasChoice anot = 
      case Anot.toMaybe anot of
        Just (Alloc.AllocationChoice _) -> True
        _                               -> False

    isManifest :: Anot.Annotation -> Bool
    isManifest anot = 
      case Anot.toMaybe anot of
        Just Alloc.Manifest -> True
        _                   -> False


-}
