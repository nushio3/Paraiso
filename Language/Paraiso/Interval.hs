{-# LANGUAGE StandaloneDeriving #-}

module Language.Paraiso.Interval (
                                  Interval(..)) where


import Language.Paraiso.Set as Set


data Interval a = EmptyInterval | Interval{lower::a, upper::a}

instance (Ord a) => Set (Interval a) where
  empty = EmptyInterval
  null EmptyInterval = True
  null (Interval l u) = l >= u
  intersection EmptyInterval _ = EmptyInterval
  intersection _ EmptyInterval = EmptyInterval
  intersection (Interval l1 u1) (Interval l2 u2) =
    let l = max l1 l2; u = min u1 u2; ret = Interval l u in
    if Set.null ret then EmptyInterval else ret

deriving instance (Eq a) => Eq (Interval a)                          
deriving instance (Show a) => Show (Interval a)   
deriving instance (Read a) => Read (Interval a)

--
