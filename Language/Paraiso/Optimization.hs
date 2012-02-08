{-# LANGUAGE CPP, DeriveDataTypeable, KindSignatures, 
MultiParamTypeClasses, RankNTypes
 #-}
{-# OPTIONS -Wall #-}

-- | tipycal optimization menu

module Language.Paraiso.Optimization (
  optimize,
  Level(..),
  Ready
  ) where

import           Data.Typeable
import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.OM (OM(..))
import           Language.Paraiso.OM.Graph (globalAnnotation)
import           Language.Paraiso.Optimization.BoundaryAnalysis
import           Language.Paraiso.Optimization.DeadCodeElimination
import           Language.Paraiso.Optimization.DecideAllocation
import           Language.Paraiso.Optimization.DependencyAnalysis
import           Language.Paraiso.Optimization.Graph
import           Language.Paraiso.Optimization.Identity
import           Language.Paraiso.Prelude



optimize :: (Ready v g)            
            => Level 
            -> OM v g Anot.Annotation 
            -> OM v g Anot.Annotation
            
optimize level om = 
  if (Just level > maybeOldLevel) 
  then recordLevel $ optimizer om 
  else om
  where
    maybeOldLevel = Anot.toMaybe $ globalAnnotation $ setup om
    
    recordLevel om = 
      om
      { setup = (setup om)
        { globalAnnotation = Anot.set (level) $ globalAnnotation (setup om)
        }
      }

    optimizer = case level of
      O0 -> gmap identity . 
            writeGrouping . 
            gmap boundaryAnalysis . 
            gmap decideAllocation . 
            gmap deadCodeElimination 
      _  -> optimize O0

data Level 
  = Unoptimized -- even mandatory code analyses are not performed
  | O0 -- perform mandatory code analyses
  | O1
  | O2
  | O3
    deriving (Eq, Ord, Show, Typeable)