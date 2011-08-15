{-# OPTIONS -Wall #-}
-- | informations for generating native codes.


module Language.Paraiso.Generator.Native (
  Setup(..), defaultSetup,
  Language(..)
  ) where

import qualified Language.Paraiso.Optimization as Opt

-- | the setups that needed to generate the native codes.
data Setup 
  = Setup 
    { language  :: Language, -- ^ the preferred native language
      directory :: FilePath, -- ^ the directory on which programs are to be generated
      optLevel  :: Opt.Level -- ^ the intensity of optimization
    }

defaultSetup :: Setup
defaultSetup 
  = Setup 
  { language = CPlusPlus,
    directory = "./",
    optLevel = Opt.O3
  }

data Language
  = CPlusPlus 
  | CUDA
  deriving (Eq, Show)

