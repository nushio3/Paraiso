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
data (Vector vector, Ring.C gauge) => POM vector gauge a = 
  POM {
    setup :: Setup vector gauge,
    kernels :: [Kernel vector gauge a]
  } 
    deriving (Show)

-- | create a POM easily and consistently.
makePOM :: (Vector v, Ring.C g) => 
           (Setup v g)              -- ^The machine configuration.
        -> [(Name, Builder v g ())] -- ^The list of pair of the kernel name and its builder.
        -> POM v g ()               -- ^The result.
makePOM setup0 kerns = 
  POM {
    setup = setup0,
    kernels = map (\(n,b) -> makeKernel setup0 n b) kerns
  }
  




