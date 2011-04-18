{-# LANGUAGE TypeOperators #-}

module Language.Paraiso.Orthotope(
  Orthotope0(..),
  Orthotope1,Orthotope2,Orthotope3
) where

import Language.Paraiso.Axis
import Language.Paraiso.PiSystem as S
import Language.Paraiso.Interval

-- | an empty, zero-dimensional orthotope
-- | a filled, zero-dimensional orthotope
data Orthotope0 a = Z0 
                  | Z  
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
  




