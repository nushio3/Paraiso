{-# LANGUAGE CPP, TypeOperators #-}

{- |  In geometry, an 'Orthotope' (also called a hyperrectangle or a box) is
  the generalization of a rectangle for higher dimensions, formally
  defined as the Cartesian product of 'Interval's.

-}

module Language.Paraiso.Orthotope(
  Orthotope0,
  Orthotope1,Orthotope2,Orthotope3
) where

import           Data.Tensor.TypeLevel
import           Language.Paraiso.Interval



type Orthotope0 a = Vec0 (Interval a)
type Orthotope1 a = Vec1 (Interval a)
type Orthotope2 a = Vec2 (Interval a)
type Orthotope3 a = Vec3 (Interval a)

