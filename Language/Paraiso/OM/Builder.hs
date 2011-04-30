{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for POM. 
module Language.Paraiso.OM.Builder() where


import qualified Data.Graph.Inductive as G
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm
import Language.Paraiso.OM.Value as Val
import Language.Paraiso.OM.Value as DVal
import Language.Paraiso.OM.Expr as E
