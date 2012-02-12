{-# LANGUAGE CPP, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Optimization.DeadCodeElimination (
  deadCodeElimination
  ) where

import           Control.Applicative
import qualified Data.Graph.Inductive                   as FGL
import           Data.Maybe
import qualified Data.Vector                            as V
import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Annotation.Execution  as Anot
import           Language.Paraiso.Prelude
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Optimization.Graph
import           Prelude hiding ((++))

-- | an optimization that changes nothing.
deadCodeElimination :: Optimization
deadCodeElimination = removeDead . markDead

markDead :: Optimization
markDead graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = Anot.set (Anot.Alive $ memoAlive V.! i) a 
              
    memoAlive :: V.Vector Bool
    memoAlive = V.generate (FGL.noNodes graph) alive
    
    alive i = case FGL.lab graph i of
      Nothing -> error $ "node [" ++ show i ++ "] disappeared"
      Just x -> case x of
        NInst (Store _)_ -> True
        NInst (Load _)_ -> True
        _ -> or $ map (memoAlive V.!) $ FGL.suc graph i
    
removeDead :: Optimization    
removeDead graph = graph2
  
  where
    graph2 = FGL.mkGraph newNodes newEdges
      

    newNodes = 
      catMaybes $
      fmap (\(idx, lab) -> (,lab) <$> renumber idx) $ 
      FGL.labNodes graph
    
    newEdges = 
      catMaybes $
      fmap (\(iFrom, iTo, lab) -> (,,lab) <$> renumber iFrom <*> renumber iTo) $ 
      FGL.labEdges graph
      
    renumber :: Int -> Maybe Int
    renumber = (oldToNew V.!)
    
    oldToNew :: V.Vector (Maybe FGL.Node)
    oldToNew = 
      V.update (V.replicate (FGL.noNodes graph) (Nothing)) $
      V.imap (\newIdx oldIdx -> (oldIdx, Just newIdx)) $
      newToOld
    newToOld :: V.Vector FGL.Node
    newToOld = 
      V.filter alive $ -- filter only alive nodes of
      V.generate (FGL.noNodes graph) id   -- all the node indices
    alive :: FGL.Node -> Bool
    alive i = case Anot.toMaybe $ getA $ fromJust $ FGL.lab graph i of
      Just (Anot.Alive x) -> x
      Nothing             -> error $ "please markDead before removeDead"