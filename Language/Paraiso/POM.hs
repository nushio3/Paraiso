{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..), makePOM
  ) where

import qualified Algebra.Ring as Ring
import Language.Paraiso.OM.Builder (Builder, makeKernel)
import Language.Paraiso.OM.Graph
import Language.Paraiso.Tensor
import NumericPrelude

-- | POM is Primordial Orthotope Machine.
data (Vector vector, Ring.C gauge) => POM vector gauge a1 a2 = 
  POM {
    pomName :: Name,
    setup :: Setup vector gauge a1,
    kernels :: [Kernel vector gauge a2]
  } 
    deriving (Show)
instance (Vector v, Ring.C g) => Named (POM v g a1 a2) where
  name = pomName

-- | create a POM easily and consistently.
makePOM :: (Vector v, Ring.C g) => 
           Name                     -- ^The machine name.
        -> (Setup v g ())           -- ^The machine configuration.
        -> [(Name, Builder v g ())] -- ^The list of pair of the kernel name and its builder.
        -> POM v g () ()            -- ^The result.
makePOM name0 setup0 kerns = 
  POM {
    pomName = name0,
    setup = setup0,
    kernels = map (\(n,b) -> makeKernel setup0 n b) kerns
  }
  




