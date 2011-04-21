{-# LANGUAGE StandaloneDeriving #-}

module Language.Paraiso.Interval (
                                  Interval(..)) where


import Language.Paraiso.PiSystem as S
import Prelude hiding (null)

data Interval a = Empty | Interval{lower::a, upper::a}

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




