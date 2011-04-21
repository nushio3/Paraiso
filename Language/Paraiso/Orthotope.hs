{-# LANGUAGE TypeOperators #-}

module Language.Paraiso.Orthotope(
  Orthotope0,
  Orthotope1,Orthotope2,Orthotope3
) where

import Language.Paraiso.Tensor
import Language.Paraiso.Interval


-- | higher dimensional orthotopes 

type Orthotope0 a = Vec0 (Interval a)
type Orthotope1 a = Vec1 (Interval a)
type Orthotope2 a = Vec2 (Interval a)
type Orthotope3 a = Vec3 (Interval a)

