{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..), makePOM
  ) where

import           Language.Paraiso.OM.Builder (Builder, buildKernel)
import           Language.Paraiso.OM.Graph
import           NumericPrelude

-- | POM is Primordial Orthotope Machine.
data POM vector gauge anot = 
  POM {
    pomName :: Name,
    setup :: Setup vector gauge anot,
    kernels :: [Kernel vector gauge anot]
  } 
    deriving (Show)

instance Nameable (POM v g a) where
  name = pomName

-- | create a POM easily and consistently.
makePOM :: 
  Name                     -- ^The machine name.
  -> (Setup v g a)              -- ^The machine configuration.
  -> [(Name, Builder v g a ())] -- ^The list of pair of the kernel name and its builder.
  -> POM v g a       -- ^The result.
makePOM name0 setup0 kerns = 
  POM {
    pomName = name0,
    setup = setup0,
    kernels = map (\(n,b) -> buildKernel setup0 n b) kerns
  }
  




