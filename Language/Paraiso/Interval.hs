{-# LANGUAGE StandaloneDeriving #-}

module Language.Paraiso.Interval (
                                  Interval(..)) where


import Language.Paraiso.Set


data Interval a = EmptyInterval | Interval{lower::a, upper::a}

instance (Ord a) => Set (Interval a) where
  empty = EmptyInterval
  intersection EmptyInterval _ = EmptyInterval
  intersection _ EmptyInterval = EmptyInterval
  intersection (Interval l1 u1) (Interval l2 u2) =
    let l = max l1 l2; u = min u1 u2 in
    if l >= u then EmptyInterval else Interval l u

deriving instance (Eq a) => Eq (Interval a)                          
deriving instance (Show a) => Show (Interval a)   
deriving instance (Read a) => Read (Interval a)

--
