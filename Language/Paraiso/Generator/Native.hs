{-# LANGUAGE CPP, FlexibleContexts, KindSignatures, StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
-- | informations for generating native codes.


module Language.Paraiso.Generator.Native (
  Setup(..), defaultSetup,
  Language(..)
  ) where

import           Data.Tensor.TypeLevel
import qualified Language.Paraiso.Optimization as Opt
import qualified Language.Paraiso.Annotation.Boundary as B

-- | the setups that needed to generate the native codes.
data Setup (vector :: * -> *) (gauge :: *)
  = Setup 
    { language     :: Language,          -- ^ the preferred native language
      directory    :: FilePath,          -- ^ the directory on which programs are to be generated
      optLevel     :: Opt.Level,         -- ^ the intensity of optimization
      localSize    :: vector gauge,      -- ^ the dimension of the physically meaningful region
      boundary     :: vector B.Condition,-- ^ the boundary condition imposed
      cudaGridSize :: (Int, Int)         -- ^ CUDA grid x block size (will be variable of subkernel in the future)
    } 

deriving instance (Show (v B.Condition), Show (v g))
 => Show (Setup v g)

defaultSetup :: (Opt.Ready v g) => v g -> Setup v g
defaultSetup sz
  = Setup 
  { language = CPlusPlus,
    directory = "./",
    optLevel = Opt.O3,
    localSize = sz,
    boundary  = compose $ const B.Open,
    cudaGridSize = (32, 32)
  }

data Language
  = CPlusPlus 
  | CUDA 
  deriving (Eq, Show)

