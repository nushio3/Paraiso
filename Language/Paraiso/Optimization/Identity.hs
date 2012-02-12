{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}
module Language.Paraiso.Optimization.Identity (
  identity
  ) where

import qualified Data.Graph.Inductive        as FGL
import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.Prelude
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Optimization.Graph

-- | an optimization that changes nothing.
identity :: Optimization
identity graph = imap update graph 
  where
    update :: FGL.Node -> Anot.Annotation -> Anot.Annotation
    update i a = const a i