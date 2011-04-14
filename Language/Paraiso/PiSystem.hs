{-# LANGUAGE TypeOperators #-}

module Language.Paraiso.PiSystem (PiSystem(..)) where
{- In mathematics, a pi-system is a non-empty family of sets that is closed
under finite intersections.  -}

import Prelude hiding (null)
import Language.Paraiso.Axis

class PiSystem a where
  empty :: a
  null :: a -> Bool
  intersection :: a -> a -> a
  
instance (PiSystem a, PiSystem b) => PiSystem (a :. b) where
  empty = empty :. empty
  null (a:.b) = null a || null b
  intersection (a1:.b1) (a2:.b2) = 
    let ret = intersection a1 a2 :. intersection b1 b2 in
    if null ret then empty else ret
                                    
