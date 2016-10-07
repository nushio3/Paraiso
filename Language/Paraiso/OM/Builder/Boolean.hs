{-# LANGUAGE CPP, FlexibleInstances, RankNTypes,
  TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
-- | An extension module of building blocks. Contains booleans, comparison operations, branchings.

module Language.Paraiso.OM.Builder.Boolean
  (eq, ne, lt, le, gt, ge, select) where

import Data.Dynamic (Typeable, typeOf)
import qualified Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.Builder.Internal
import Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm as Realm
import Language.Paraiso.OM.Value as Val


infix 4 `eq`, `ne`, `lt`, `le`, `gt`, `ge`

-- | generate a binary operator that returns Bool results.
mkOp2B :: (TRealm r, Typeable c) => 
          A.Operator                   -- ^The operation to be performed
       -> (Builder v g a (Value r c))    -- ^The first argument
       -> (Builder v g a (Value r c))    -- ^The second argument
       -> (Builder v g a (Value r Bool)) -- ^The result
mkOp2B op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNodeE [n1, n2] $ NInst (Arith op) 
  n01 <- addNodeE [n0]    $ NValue (toDyn v1){typeRep = typeOf True} 
  return $ FromNode r1 True n01


type CompareOp = forall r c v g a. (TRealm r, Typeable c) => 
    (Builder v g a (Value r c)) -> (Builder v g a (Value r c)) -> (Builder v g a (Value r Bool))

-- | Equal
eq :: CompareOp
eq = mkOp2B A.EQ
-- | Not equal
ne :: CompareOp
ne = mkOp2B A.NE
-- | Less than
lt :: CompareOp
lt = mkOp2B A.LT
-- | Less than or equal to
le :: CompareOp
le = mkOp2B A.LE
-- | Greater than
gt :: CompareOp
gt = mkOp2B A.GT
-- | Greater than or equal to
ge :: CompareOp
ge = mkOp2B A.GE

-- | selects either the second or the third argument based 
select ::(TRealm r, Typeable c) => 
         (Builder v g a (Value r Bool)) -- ^The 'Bool' condition
      -> (Builder v g a (Value r c))    -- ^The value chosen when the condition is 'True'
      -> (Builder v g a (Value r c))    -- ^The value chosen when the condition is 'False'
      -> (Builder v g a (Value r c))    -- ^The result
select builderB builder1 builder2 = do
  vb <- builderB
  v1 <- builder1
  v2 <- builder2
  nb <- valueToNode vb
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNodeE [nb, n1, n2] $ NInst (Arith A.Select) 
  n01 <- addNodeE [n0] $ NValue (toDyn v1) 
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  return $ FromNode r1 c1 n01

