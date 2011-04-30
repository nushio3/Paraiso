{-# OPTIONS -Wall #-}
module Language.Paraiso.OM.Reduce (Operator(..)) where

data Operator = Max | Min | Sum deriving (Eq, Ord, Show, Read)
