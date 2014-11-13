{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}

import qualified Data.Vector as V
import Data.Ratio
import Data.List
import System.Process
import Text.Printf
import Linear

type Pt r = V3 r
type PtR = Pt Rational
data Axis = X | Y | Z deriving (Eq, Ord)

instance Show Axis where
  show X = "x"
  show Y = "y"
  show Z = "z"

type Field r a = Pt r -> a
type Tensor1 a = Axis -> a
type Tensor2 a = (Axis,Axis) -> a


asT1 :: V3 a -> Tensor1 a
asT1 (V3 x _ _) X = x
asT1 (V3 _ y _) Y = y
asT1 (V3 _ _ z) Z = z

asT2 :: V3 (V3 a) -> Tensor2 a
asT2 = uncurry . fmap asT1 . asT1 

sigma :: (Show r) => Tensor2 (Field r String)
sigma (a,b) r = printf "\\sigma_{%s,%s}[%s]" (show $ min a b) (show $ max a b) (show r)

partial :: forall a r. (Fractional a, Fractional r) => Tensor1 (Field r a -> Field r a)
partial i f p = (f (p + 0.5 *^ e(i)) - f (p - 0.5 *^ e(i))) * (fromRational $ 9%8)
              - (f (p + 1.5 *^ e(i)) - f (p - 1.5 *^ e(i))) * (fromRational $ 1%24)
  where
    e :: Tensor1 (Pt r)
    e = asT1 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) 

type VarName = String
data Op2 = Add | Mul | Sub 
instance Show Op2 where
  show Add = "+"         
  show Mul = " "         
  show Sub = "-"         

data Expr a where
  Var :: VarName -> Expr x
  AVar :: VarName -> Expr Axis
  Imm :: Double -> Expr x
  Expr2 :: Op2 -> Expr x -> Expr x -> Expr x
  Apply :: Expr (a->b) -> Expr a -> Expr b

instance Eq (Expr a) where
  AVar x == AVar y = x == y
  _ == _ = False

instance Num (Expr a) where
  a+b = Expr2 Add a b
  a-b = Expr2 Sub a b
  a*b = Expr2 Mul a b
  fromInteger n = Imm (fromInteger n)
  abs = undefined
  signum = undefined

