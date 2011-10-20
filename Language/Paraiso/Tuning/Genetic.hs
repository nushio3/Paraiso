{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Tuning.Genetic
  (
   Genome, Species(..),
   makeSpecies,
   readGenome, overwriteGenome,
   mutate, 
   generateIO
  ) where

import qualified Control.Monad.State as State
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Generator.Native as Native
import qualified Language.Paraiso.OM as OM
import qualified Language.Paraiso.OM.Graph as OM
import qualified Language.Paraiso.Optimization          as Opt
import           Language.Paraiso.Prelude
import qualified Language.Paraiso.Generator as Gen  (generateIO) 
import qualified Text.Read as Read

data Species v g = 
  Species {
    setup   :: Native.Setup v g,
    machine :: OM.OM v g Anot.Annotation
  } deriving (Show)

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

mutate :: Genome -> Genome
mutate (Genome xs) = Genome $ map not xs

readGenome :: Species v g -> Genome
readGenome spec =
  encode $ do
    let (x,y) = Native.cudaGridSize $ setup spec
    putInt 16 x
    putInt 16 y

overwriteGenome :: Genome -> Species v g -> Species v g
overwriteGenome dna oldSpec = 
  decode dna $ do
    x <- getInt 16
    y <- getInt 16
    let oldSetup = setup oldSpec
        oldOM = machine oldSpec
    let newGrid = (x,y)
        newSetup = oldSetup {Native.cudaGridSize = newGrid}
        newOM = oldOM
    return $ Species newSetup newOM



newtype Get a = Get { getGet :: State.State [Bool] a } deriving (Monad)
newtype Put a = Put { getPut :: State.State [Bool] a } deriving (Monad)

get :: Get Bool
get = Get $ do
  (x:xs) <- State.get
  State.put xs
  return x

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
