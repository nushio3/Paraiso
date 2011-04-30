{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
module Language.Paraiso.OM.Builder
    (
     Builder(..)
    ) where

import qualified Algebra.Ring as Ring
import Control.Monad.State 
import qualified Data.Graph.Inductive as G
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm
import Language.Paraiso.OM.Value as Val
import Language.Paraiso.OM.Value as DVal
import Language.Paraiso.OM.Expr as E
import Language.Paraiso.Tensor

type Builder vector gauge a val = 
  State (OMGraph vector gauge a) val