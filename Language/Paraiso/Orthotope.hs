{-# LANGUAGE TypeOperators #-}

module Language.Paraiso.Orthotope(
  Z(..),
  Orthotope0,Orthotope1,Orthotope2,Orthotope3
) where

import Language.Paraiso.Set as Set
import Language.Paraiso.Interval
import Data.Array.Repa ((:.)(..))

data Z a = Z0 | Z deriving (Eq,Ord,Show,Read)

type Orthotope0 a = Z a
type Orthotope1 a = Orthotope0 a :. Interval a
type Orthotope2 a = Orthotope1 a :. Interval a
type Orthotope3 a = Orthotope2 a :. Interval a

instance Set (Z a) where
  empty = Z0
  null Z0 = True
  null Z = False
  intersection Z Z = Z
  intersection _ _ = Z0
  
instance (Set a, Set b) => Set (a :. b) where
  empty = empty :. empty
  null (a:.b) = Set.null a || Set.null b
  intersection (a1:.b1) (a2:.b2) = 
    let ret = intersection a1 a2 :. intersection b1 b2 in
    if Set.null ret then empty else ret
                                    
