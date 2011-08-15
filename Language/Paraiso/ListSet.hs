{-# OPTIONS -Wall #-}

-- |  A list with its elements are assured to be unique and are soreted.

module Language.Paraiso.ListSet (
  Set, fromList, toList, union, unionAll

  ) where

import qualified Data.List.Ordered as O

newtype Set a = Set { toList :: [a] }
                  deriving (Eq, Ord, Show, Read)
                           
fromList :: Ord a => [a] -> Set a                           
fromList = Set . O.nub . O.sort

union :: Ord a => Set a -> Set a ->  Set a 
union xs ys = Set $ O.union (toList xs) (toList ys)

unionAll :: Ord a => [Set a] -> Set a
unionAll = Set . O.unionAll . map toList
