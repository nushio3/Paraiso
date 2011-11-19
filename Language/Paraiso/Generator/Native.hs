{-# LANGUAGE KindSignatures #-}
{-# OPTIONS -Wall #-}
-- | informations for generating native codes.


module Language.Paraiso.Generator.Native (
  Setup(..), defaultSetup,
  Language(..)
  ) where

import qualified Language.Paraiso.Optimization as Opt

-- | the setups that needed to generate the native codes.
data Setup (vector :: * -> *) (gauge :: *)
  = Setup 
    { language  :: Language, -- ^ the preferred native language
      directory :: FilePath, -- ^ the directory on which programs are to be generated
      optLevel  :: Opt.Level, -- ^ the intensity of optimization
      localSize :: vector gauge, -- ^ the dimension of the physically meaningful region
      cudaGridSize :: (Int, Int) -- ^ CUDA grid x block size (will be variable of subkernel in the future)
    } deriving (Show)

defaultSetup :: (Opt.Ready v g) => v g -> Setup v g
defaultSetup sz
  = Setup 
  { language = CPlusPlus,
    directory = "./",
    optLevel = Opt.O3,
    localSize = sz,
    cudaGridSize = (128, 128)
  }

data Language
  = CPlusPlus 
  | CUDA 
  deriving (Eq, Show)

