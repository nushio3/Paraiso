{-# LANGUAGE StandaloneDeriving #-}
{- | an 'Interval' is a pair of 'lower' and 'upper', 
   representing some interval in ordered system.
   The lower bound is inclusive and the upper bound is exclusive:
   ('lower' <= x <  'upper') .
   The intersection of two intervals are also interval 
   but the union of two intervals are not, 
   so 'Interval' constitute a 'PiSystem'.
 -}

module Language.Paraiso.Interval (
                                  Interval(..)) where


import Language.Paraiso.PiSystem as S
import Prelude hiding (null)

data Interval a = 
    -- | an empty interval.
    Empty | 
    -- | a non-empty interval.
    Interval{lower::a, upper::a}

instance (Ord a) => PiSystem (Interval a) where
  empty = Empty
  null Empty = True
  null (Interval l u) = l >= u
  intersection Empty _ = Empty
  intersection _ Empty = Empty
  intersection (Interval l1 u1) (Interval l2 u2) =
    let l = max l1 l2; u = min u1 u2; ret = Interval l u in
    if null ret then Empty else ret

deriving instance (Eq a) => Eq (Interval a)                          
deriving instance (Show a) => Show (Interval a)   
deriving instance (Read a) => Read (Interval a)




