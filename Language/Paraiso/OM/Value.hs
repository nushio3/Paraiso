{-# OPTIONS -Wall #-}

-- | The 'Value' is flowing through the OM dataflow graph.
-- 'Value' carries the type and homogeneity information about the dataflow as Type.
-- Therefore, operation between 'Value' with wrong type will raise type errors.

module Language.Paraiso.OM.Value
  (
   Value(..), StaticValue(..)
  ) where

import Data.Typeable
import qualified Data.Graph.Inductive as G
import qualified Language.Paraiso.OM.Realm as R


-- | value type, with its realm and content type discriminated in type level
data
  Value rea con = 
  -- | data obtained from the dataflow graph.
  -- 'realm' carries a type-level realm information, 
  -- 'content' carries only type information and its ingredient is nonsignificant
  -- and can be 'undefined'.
  FromNode {realm :: rea, content :: con, node :: G.Node} | 
  -- | data obtained as an immediate value.
  -- 'realm' carries a type-level realm information, 
  -- 'content' is the immediate value to be stored.
  FromImm {realm :: rea, content :: con} deriving (Eq, Show)
                       
-- | static value type.
data StaticValue rea con = StaticValue rea con deriving (Eq, Show)


instance  (R.TRealm rea, Typeable con) => R.Realmable (Value rea con) where
  realm (FromNode r _ _) = R.tRealm r
  realm (FromImm r _) = R.tRealm r
  


