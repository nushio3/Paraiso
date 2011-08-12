{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.OM
  (
    OM(..), makeOM
  ) where

import           Language.Paraiso.Name
import           Language.Paraiso.OM.Builder (Builder, buildKernel)
import           Language.Paraiso.OM.Graph
import           NumericPrelude

-- | POM is Primordial Orthotope Machine.
data OM vector gauge anot 
  = OM 
    { omName :: Name,
      setup :: Setup vector gauge anot,
      kernels :: [Kernel vector gauge anot]
    } 
      deriving (Show)

instance Nameable (OM v g a) where
  name = omName

-- | create a POM easily and consistently.
makeOM :: 
  Name                     -- ^The machine name.
  -> (Setup v g a)              -- ^The machine configuration.
  -> [(Name, Builder v g a ())] -- ^The list of pair of the kernel name and its builder.
  -> OM v g a       -- ^The result.
makeOM name0 setup0 kerns 
  = OM {
    omName = name0,
    setup = setup0,
    kernels = map (\(n,b) -> buildKernel setup0 n b) kerns
  }
  




