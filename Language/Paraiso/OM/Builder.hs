{-# LANGUAGE CPP, FlexibleInstances, RankNTypes, TypeSynonymInstances  #-}
{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
-- This module just exports a set of chosen symbols
-- from 'Language.Paraiso.OM.Builder.Internal'.

module Language.Paraiso.OM.Builder
    (
     Builder, BuilderState(..),
     BuilderOf,
     buildKernel,
     
     bind,
     load, store, 
     reduce, broadcast, 
     loadIndex, loadSize, 
     shift, imm, 
     cast, castTo,
     annotate, (<?>),
     withAnnotation
    ) where

import Language.Paraiso.OM.Builder.Internal
