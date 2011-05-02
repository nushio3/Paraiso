{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An extension module of building blocks. Contains booleans, comparison operations, branchings.

module Language.Paraiso.OM.Builder.Boolean
  (eq, ne, lt, le, gt, ge, select) where


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


-- | generate a binary operator that returns Bool results.
mkOp2B :: (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
          A.Operator                   -- ^The operation to be performed
       -> (Builder v g (Value r c))    -- ^The first argument
       -> (Builder v g (Value r c))    -- ^The second argument
       -> (Builder v g (Value r Bool)) -- ^The result
mkOp2B op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNode [n1, n2] (NInst (Arith op) ())
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  return $ FromNode r1 True n01


eq, ne, lt, le, gt, ge :: (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
                          (Builder v g (Value r c)) -> (Builder v g (Value r c)) -> (Builder v g (Value r Bool))

-- | Equal
eq = mkOp2B A.EQ
-- | Not equal
ne = mkOp2B A.NE
-- | Less than
lt = mkOp2B A.LT
-- | Less than or equal to
le = mkOp2B A.LE
-- | Greater than
gt = mkOp2B A.GT
-- | Greater than or equal to
ge = mkOp2B A.GE

-- | selects either the second or the third argument based 
select ::(Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
         (Builder v g (Value r Bool)) -- ^The 'Bool' condition
      -> (Builder v g (Value r c))    -- ^The value chosen when the condition is 'True'
      -> (Builder v g (Value r c))    -- ^The value chosen when the condition is 'False'
      -> (Builder v g (Value r c))    -- ^The result
select builderB builder1 builder2 = do
  vb <- builderB
  v1 <- builder1
  v2 <- builder2
  nb <- valueToNode vb
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNode [nb, n1, n2] (NInst (Arith A.Select) ())
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  return $ FromNode r1 c1 n01

