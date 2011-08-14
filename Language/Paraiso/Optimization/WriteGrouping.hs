{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Optimization.WriteGrouping (
  writeGrouping
  ) where

import qualified Data.Vector                 as V
import qualified Data.Graph.Inductive        as FGL
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Anot
import           Language.Paraiso.Prelude
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Optimization.Graph

-- | an optimization that changes nothing.
writeGrouping :: Optimization
writeGrouping graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = const a i
    
    -- maps index of ManifestNode to the index in the graph
    midxToIdx :: V.Vector Int
    midxToIdx =  V.map fst $ 
                 V.filter snd $ 
                 V.imap (\idx isManifest' -> (idx, isManifest')) $ 
                 isManifest 
    midxSize = V.length midxToIdx
    
    isManifest :: V.Vector Bool
    isManifest = V.generate (FGL.noNodes graph) $ \idx ->
      case FGL.lab graph idx of
        Nothing -> error $ "Node index [" ++ show idx ++ "] missing."
        Just nd -> case (Anot.toMaybe $ getA nd) of
          Just Anot.Manifest -> True
          Just _             -> False
          Nothing            -> error "writeGrouping must be done after decideAllocation"
          


    -- (dependMatrix ! idx ! midx) whether Node idx depends on ManifestNode midx
    dependMatrix :: V.Vector (V.Vector Bool)
    dependMatrix = V.generate (FGL.noNodes graph) dependRow

    dependRow idx 
      -- a manifest node depend only on itself
      | isManifest V.! idx = V.map (==idx) midxToIdx 
      -- a non-manifest node depends on its predecessors
      | otherwise          = foldl mergeRow allFalseRow $ map (dependMatrix V.!) $ FGL.pre graph idx
                             
    allFalseRow = V.replicate midxSize False
    mergeRow va vb
      | V.length va /= midxSize = error "wrong size contamination in dependMatrix"
      | V.length vb /= midxSize = error "wrong size contamination in dependMatrix" 
      | otherwise               = V.zipWith (||) va vb
