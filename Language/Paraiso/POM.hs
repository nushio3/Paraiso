{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.POM
  (
   POM(..), makePOM, mapGraph
  ) where

import qualified Algebra.Ring as Ring
import qualified Control.Monad as Monad
import           Language.Paraiso.Annotation (Annotation)
import           Language.Paraiso.OM.Builder (Builder, buildKernel)
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Tensor
import           NumericPrelude

-- | POM is Primordial Orthotope Machine.
data (Vector vector, Ring.C gauge) => POM vector gauge a = 
  POM {
    pomName :: Name,
    setup :: Setup vector gauge,
    kernels :: [Kernel vector gauge a]
  } 
    deriving (Show)

instance (Vector v, Ring.C g) => Nameable (POM v g a) where
  name = pomName

instance (Vector v, Ring.C g) => Monad.Functor (POM v g) where
  fmap = mapGraph . nmap 

-- | modify each of the graphs in POM.
mapGraph :: (Vector v, Ring.C g) => 
            (Graph v g a -> Graph v g b)
         -> POM v g a         
         -> POM v g b
mapGraph f pom = pom
                 { kernels = map 
                   (\kern -> kern{dataflow = f $ dataflow kern}) $ 
                   kernels pom}


-- | create a POM easily and consistently.
makePOM :: 
  (Vector v, Ring.C g) 
  => Name                     -- ^The machine name.
  -> (Setup v g)              -- ^The machine configuration.
  -> [(Name, Builder v g ())] -- ^The list of pair of the kernel name and its builder.
  -> POM v g Annotation       -- ^The result.
makePOM name0 setup0 kerns = 
  POM {
    pomName = name0,
    setup = setup0,
    kernels = map (\(n,b) -> buildKernel setup0 n b) kerns
  }
  




