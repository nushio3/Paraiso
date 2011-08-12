{-# OPTIONS -Wall #-}
-- | informations for generating native codes.


module Language.Paraiso.Generator.Native (
  Setup(..), defaultSetup,
  Language(..)
  ) where

data Setup 
  = Setup 
    { language  :: Language,
      directory :: FilePath
    }

defaultSetup :: Setup
defaultSetup 
  = Setup 
  { language = CPlusPlus,
    directory = "./"
  }

data Language
  = CPlusPlus 
  | CUDA
  deriving (Eq, Show)
