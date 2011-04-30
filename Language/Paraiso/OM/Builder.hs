{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
-- Builder is only for Graph vector gauge () . 
-- Graphs with other annotation types can be created by fmap.
module Language.Paraiso.OM.Builder
    (
     Builder(..), BuilderState(..),
     initState
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

data BuilderState vector gauge = BuilderState 
    { setup :: Setup vector gauge, 
      target :: Graph vector gauge ()} deriving (Show)

initState :: Setup v g -> BuilderState v g
initState s = BuilderState {
                setup = s,
                target = G.empty
              }


type Builder vector gauge val = 
  State (BuilderState vector gauge) val
  
