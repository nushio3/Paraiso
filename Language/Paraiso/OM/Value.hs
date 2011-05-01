{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the OM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow.

module Language.Paraiso.OM.Value
  (
   Value(..)
  ) where

import Data.Typeable
import qualified Data.Graph.Inductive as G
import qualified Language.Paraiso.OM.Realm as R


-- | value type, with its realm and content type discriminated in type level
data (R.TRealm rea, Typeable con) => 
  Value rea con = 
  -- | data obtained from the dataflow graph
  FromNode {realm :: rea, content :: con, node :: G.Node} | 
  -- | data obtained as an immediate value
  FromImm {realm :: rea, content :: con} deriving (Eq, Show)
                       

instance  (R.TRealm rea, Typeable con) => R.Realmable (Value rea con) where
  realm (FromNode r _ _) = R.realm r
  realm (FromImm r _) = R.realm r
  

