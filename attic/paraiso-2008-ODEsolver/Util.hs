
module Util(
            partitionN, (+:), groupSort, zipWith3M_, zipWith6M_
) where

import Data.List
import Control.Monad

partitionN::[Int]->(a->Int)->[a]->[[a]]

partitionN labels label xs = [filter ((==l).label) xs | l <- labels] 


xs +: x = xs ++ [x]
infix 1 +: 

groupSort::(Eq t, Ord t)=>(a->t)->[a]->[(t,[a])]
groupSort _ [] = []
groupSort tag xs = 
    map (\xs -> (tag.head $ xs, xs)) $
    groupBy (\a b -> tag a == tag b) $
    sortBy (\a b -> tag a `compare` tag b) $ xs

zipWith3M_ f as bs cs = sequence_ $ zipWith3 f as bs cs
zipWith6M_ f as bs cs ds es fs = sequence_ $ zipWith6 f as bs cs ds es fs