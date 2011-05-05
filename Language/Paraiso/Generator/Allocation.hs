{-# OPTIONS -Wall #-}
module Language.Paraiso.Generator.Allocation(Allocation(..)) where
data Allocation = Manifest | Delayed | Auto deriving (Eq, Show)
