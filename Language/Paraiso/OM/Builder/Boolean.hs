{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An extension module of building blocks. Contains booleans, comparison operations, branchings.

module Language.Paraiso.OM.Builder.Boolean
  (eq, ne, lt, le, gt, ge) where


import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive
import Data.Dynamic (Typeable)
import qualified Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Builder.Internal
import Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm as Realm
import Language.Paraiso.OM.Value as Val
import Language.Paraiso.Tensor
import NumericPrelude


mkOp2B :: (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
          A.Operator -> (Builder v g (Value r c)) -> (Builder v g (Value r c)) -> (Builder v g (Value r Bool))
mkOp2B op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNode [n1, n2] (NInst (Arith op))
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  return $ FromNode r1 True n01


eq, ne, lt, le, gt, ge ::  (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
                           (Builder v g (Value r c)) -> (Builder v g (Value r c)) -> (Builder v g (Value r Bool))

eq = mkOp2B A.EQ
ne = mkOp2B A.NE
lt = mkOp2B A.LT
le = mkOp2B A.LE
gt = mkOp2B A.GT
ge = mkOp2B A.GE
