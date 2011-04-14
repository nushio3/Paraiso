{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
module Language.Paraiso.Axis
    (
     (:.)(..), Vec(..), Vector(..)
    ) where

import Data.Array.Repa ((:.)(..))

data Axis = Axis Int deriving (Eq,Ord,Show,Read)

data Vec = Vec deriving (Eq,Ord,Show,Read)

class Vector v a where
  getComponent :: v a -> Axis -> a

instance Vector ((:.) Vec ) a where
  getComponent (Vec :. x) _ = x