{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the POM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.POM.Value
  (
   Value(..)
  ) where

import Data.Typeable
import qualified Data.Graph.Inductive as G
import qualified Language.Paraiso.POM.Realm as R


-- | value type, with its realm and content type discriminated in type level
data (R.TRealm rea, Typeable con) => 
  Value rea con = 
  -- | data obtained from the dataflow graph
  FromNode rea G.Node | 
  -- | data obtained as an immediate value
  Imm rea con deriving (Eq, Show)
                       

instance  (R.TRealm rea, Typeable con) => R.Realmable (Value rea con) where
  realm (FromNode r _) = R.realm r
  realm (Imm r _) = R.realm r
  

