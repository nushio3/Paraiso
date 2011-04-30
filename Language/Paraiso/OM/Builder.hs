{-# LANGUAGE RankNTypes #-}
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
import qualified Control.Monad.State as State
import qualified Data.Graph.Inductive as FGL
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
                target = FGL.empty
              }



type Builder vector gauge val = 
  State.State (BuilderState vector gauge) val
  
type B a = (Vector v, Ring.C g) => Builder v g a

modifyG :: (Vector v, Ring.C g) => (Graph v g () -> Graph v g ()) -> Builder v g ()
modifyG f = State.modify (\bs -> bs{target = f.target $ bs})

getG :: (Vector v, Ring.C g) => Builder v g (Graph v g ())
getG = fmap target State.get

newNode :: B FGL.Node
newNode = do
  n <- fmap (head . FGL.newNodes 1) getG
  return n
  
load :: r -> c -> Name -> B (Value r c)
load r c = undefined
  
