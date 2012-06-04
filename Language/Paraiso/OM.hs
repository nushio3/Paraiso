{-# LANGUAGE CPP,  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.OM
  (
    OM(..), makeOM
  ) where

import qualified Data.Vector as V
import           Language.Paraiso.Name
import           Language.Paraiso.OM.Builder (Builder, buildKernel)
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.DynValue (DynValue)
import           NumericPrelude

-- | POM is Primordial Orthotope Machine.
data OM vector gauge anot 
  = OM 
    { omName :: Name,
      setup :: Setup vector gauge anot,
      kernels :: V.Vector (Kernel vector gauge anot)
    } 
      deriving (Show)

instance Nameable (OM v g a) where
  name = omName

-- | create a POM easily and consistently.
makeOM :: 
  Name                          -- ^The machine name.
  -> a                          -- ^The annotation at the root level.
  -> [Named DynValue]           -- ^The list of static variables.
  -> [Named (Builder v g a ())] -- ^The list of pair of the kernel name and its builder.
  -> OM v g a                   -- ^The result.
makeOM name0 a0 vars0 kerns 
  = OM {
    omName  = name0,
    setup   = setup0,
    kernels = V.fromList $ map (\(Named n b) -> buildKernel setup0 n b) kerns
  }
  where
    setup0 = Setup { staticValues = V.fromList vars0, globalAnnotation = a0 }