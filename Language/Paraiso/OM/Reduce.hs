{-# OPTIONS -Wall #-}
module Language.Paraiso.OM.Reduce 
  (Operator(..),
  toArith
  ) where

import qualified Language.Paraiso.OM.Arithmetic as A

data Operator = Max | Min | Sum deriving (Eq, Ord, Show, Read)


toArith :: Operator -> A.Operator
toArith op = case op of
               Max -> A.Max
               Min -> A.Min
               Sum -> A.Add
