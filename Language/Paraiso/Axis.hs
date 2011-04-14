module Language.Paraiso.Axis
    (
     (:.)(..)
    ) where

import Data.Array.Repa ((:.)(..))

data Axis = Axis Int

class Axisee vec where
  getComponent :: Axis -> vec a -> a
  

