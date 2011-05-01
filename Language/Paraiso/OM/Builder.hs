{-# LANGUAGE FlexibleInstances, RankNTypes, TypeSynonymInstances  #-}
{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
-- Builder is only for Graph vector gauge () . 
-- Graphs with other annotation types can be created by fmap.
module Language.Paraiso.OM.Builder
    (
     Builder, BuilderState(..),
     initState,
     load, store
    ) where
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Graph.Inductive as FGL
import Data.Dynamic (Typeable, typeOf)
import qualified Data.Dynamic as Dynamic
import qualified Language.Paraiso.OM.Arithmetic as A
import Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm as Realm
import Language.Paraiso.OM.Value as Val
import Language.Paraiso.Tensor

data BuilderState vector gauge = BuilderState 
    { setup :: Setup vector gauge, 
      target :: Graph vector gauge ()} deriving (Show)

initState :: Setup v g -> BuilderState v g
initState s = BuilderState {
                setup = s,
                target = FGL.empty
              }



type Builder vector gauge val = 
  State.State (BuilderState vector gauge) val
  
type B a = (Vector v, Ring.C g) => Builder v g a

modifyG :: (Vector v, Ring.C g) => (Graph v g () -> Graph v g ()) -> Builder v g ()
modifyG f = State.modify (\bs -> bs{target = f.target $ bs})

getG :: (Vector v, Ring.C g) => Builder v g (Graph v g ())
getG = fmap target State.get

newNode :: B FGL.Node
newNode = do
  n <- fmap (FGL.noNodes) getG
  return n
  
addNode :: (Vector v, Ring.C g) => [FGL.Node] -> Node v g () -> Builder v g FGL.Node
addNode froms new = do
  n <- newNode
  modifyG (([], n, new, [((), nn) | nn <-froms]) FGL.&)
  return n

valueToNode :: (TRealm r, Typeable c) => Value r c -> B FGL.Node
valueToNode val = do
  let 
      con = Val.content val
      type0 = toDyn val
  case val of
    FromNode _ _ n -> return n
    FromImm _ _ -> do
             n0 <- addNode [] (NInst $ Imm (typeOf con) (Dynamic.toDyn con))
             n1 <- addNode [n0] (NValue type0 ())
             return n1

lookUpStatic :: NamedValue -> B ()
lookUpStatic (NamedValue name0 type0)= do
  st <- State.get 
  let
      vs :: [NamedValue]
      vs = staticValues $ setup st
      matches = filter ((==name0).name) vs
      (NamedValue _ type1) = head matches
  when (length matches == 0) $ fail ("no name found: " ++ nameStr name0)
  when (length matches > 1) $ fail ("multiple match found:" ++ nameStr name0)
  when (type0 /= type1) $ fail ("type mismatch; expected: " ++ show type1 ++ "; " ++
                                " actual: " ++ nameStr name0 ++ "::" ++ show type0)

load :: (TRealm r, Typeable c) => r -> c -> Name -> B (Value r c)
load r0 c0 name0 = do
  let 
      type0 = mkDyn r0 c0
      nv = NamedValue name0 type0
  lookUpStatic nv
  n0 <- addNode [] (NInst (Load name0))
  n1 <- addNode [n0] (NValue type0 ())
  return (FromNode r0 c0 n1)

store :: (TRealm r, Typeable c) => Name -> Value r c -> B ()
store name0 val0 = do
  let 
      type0 = toDyn val0
      nv = NamedValue name0 type0
  lookUpStatic nv
  n0 <- valueToNode val0
  _ <- addNode [n0] (NInst (Store name0))
  return ()



instance (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => Additive.C (Builder v g (Value r c)) where
  zero = return $ FromImm undefined Additive.zero
  builder1 + builder2 = do
    (FromNode r1 c1 n1)  <- builder1
    (FromNode _  _  n2)  <- builder2
    n3 <- addNode [n1, n2] (NInst (Arith A.Add))
    return $ FromNode r1 c1 n3
    
    



  
{-

-- | expression Tree for type a
module Language.Paraiso.OM.Expr (Expr(..)) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Language.Paraiso.OM.Arithmetic as A
import NumericPrelude

data Expr a = Term a | Expr A.Operator [Expr a]

instance Additive.C a => Additive.C (Expr a) where
  zero     = Term zero
  x + y    = Expr A.Add [x, y]
  x - y    = Expr A.Sub [x, y]
  negate x = Expr A.Neg [x]
  
instance Ring.C a => Ring.C (Expr a) where
  one         = Term one
  x * y       = Expr A.Mul [x, y]
  fromInteger = Term . fromInteger
  x ^ y       = Expr Ipow [x, fromInteger y]
  

-}