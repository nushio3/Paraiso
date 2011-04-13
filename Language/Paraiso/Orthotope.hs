{-# LANGUAGE TypeOperators #-}

module Language.Paraiso.Orthotope(
  Orthotope0(..),
  Orthotope1,Orthotope2,Orthotope3
) where

import Language.Paraiso.PiSystem as S
import Language.Paraiso.Interval
import Data.Array.Repa ((:.)(..))

data Orthotope0 a = Z0 -- | an empty, zero-dimensional orthotope
                  | Z  -- | a filled, zero-dimensional orthotope
                    deriving (Eq,Ord,Show,Read)

-- | higher dimensional orthotopes 
type Orthotope1 a = Orthotope0 a :. Interval a
type Orthotope2 a = Orthotope1 a :. Interval a
type Orthotope3 a = Orthotope2 a :. Interval a

instance PiSystem (Orthotope0 a) where
  empty = Z0
  null Z0 = True
  null Z = False
  intersection Z Z = Z
  intersection _ _ = Z0
  
instance (PiSystem a, PiSystem b) => PiSystem (a :. b) where
  empty = empty :. empty
  null (a:.b) = S.null a || S.null b
  intersection (a1:.b1) (a2:.b2) = 
    let ret = intersection a1 a2 :. intersection b1 b2 in
    if S.null ret then empty else ret
                                    
