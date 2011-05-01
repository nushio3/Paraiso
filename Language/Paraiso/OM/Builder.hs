{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, RankNTypes, TypeSynonymInstances  #-}
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
import qualified Algebra.Field as Field
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
import NumericPrelude
import qualified Prelude (Num(..), Fractional(..))

data BuilderState vector gauge = BuilderState 
    { setup :: Setup vector gauge, 
      target :: Graph vector gauge ()} deriving (Show)

-- | Create an initial state for 'Builder' monad from a OM 'Setup'.
initState :: Setup v g -> BuilderState v g
initState s = BuilderState {
                setup = s,
                target = FGL.empty
              }

-- | The 'Builder' monad is used to build 'Kernel's.
type Builder vector gauge val = 
  State.State (BuilderState vector gauge) val
  
-- | 'Builder' needs to be an instance of 'Eq' to become an instance of  'Prelude.Num'  
instance Eq (Builder v g v2) where
  _ == _ = undefined
-- | 'Builder' needs to be an instance of 'Show' to become an instance of  'Prelude.Num'  
instance Show (Builder v g v2) where
  show _ = "<<REDACTED>>"

type B a = (Vector v, Ring.C g) => Builder v g a

-- | Modify the dataflow graph stored in the 'Builder'.
modifyG :: (Vector v, Ring.C g) => 
           (Graph v g () -> Graph v g ()) -- ^The graph modifying function.
               -> Builder v g ()          -- ^The state gets silently modified.
modifyG f = State.modify (\bs -> bs{target = f.target $ bs})

-- | Get the graph stored in the 'Builder'.
getG :: (Vector v, Ring.C g) => Builder v g (Graph v g ())
getG = fmap target State.get

-- | get the number of the next unoccupied 'FGL.Node' in the graph.
freeNode :: B FGL.Node
freeNode = do
  n <- fmap (FGL.noNodes) getG
  return n
  
-- | add a node to the graph.
addNode :: (Vector v, Ring.C g) => 
           [FGL.Node]     -- ^The list of node indicies that the new nodes depend upon.
           -> Node v g () -- ^The new node to be added
           -> Builder v g FGL.Node
addNode froms new = do
  n <- freeNode
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


imm :: (TRealm r, Typeable c) => c -> B (Value r c)
imm c0 = return (FromImm unitTRealm c0)

mkOp1 :: (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
         A.Operator -> (Builder v g (Value r c)) -> (Builder v g (Value r c))
mkOp1 op builder1 = do
  v1 <- builder1
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n0 <- addNode [n1] (NInst (Arith op))
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  return $ FromNode r1 c1 n01

mkOp2 :: (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => 
         A.Operator -> (Builder v g (Value r c)) -> (Builder v g (Value r c)) -> (Builder v g (Value r c))
mkOp2 op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNode [n1, n2] (NInst (Arith op))
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  return $ FromNode r1 c1 n01


instance (Vector v, Ring.C g, TRealm r, Typeable c, Additive.C c) => Additive.C (Builder v g (Value r c)) where
  zero = return $ FromImm unitTRealm Additive.zero
  (+) = mkOp2 A.Add
  (-) = mkOp2 A.Sub
  negate = mkOp1 A.Neg
    
instance (Vector v, Ring.C g, TRealm r, Typeable c, Ring.C c) => Ring.C (Builder v g (Value r c)) where
  one = return $ FromImm unitTRealm Ring.one
  (*) = mkOp2 A.Mul
  fromInteger = imm . fromInteger
  
instance (Vector v, Ring.C g, TRealm r, Typeable c, Ring.C c) => Prelude.Num (Builder v g (Value r c)) where  
  (+) = (Additive.+)
  (*) = (Ring.*)
  (-) = (Additive.-)
  negate = Additive.negate
  abs = undefined
  signum = undefined
  fromInteger = Ring.fromInteger
  
instance (Vector v, Ring.C g, TRealm r, Typeable c, Field.C c) => Field.C (Builder v g (Value r c)) where
  (/) = mkOp2 A.Div
  recip = mkOp1 A.Inv
  fromRational' = imm . fromRational'

instance (Vector v, Ring.C g, TRealm r, Typeable c, Field.C c, Prelude.Fractional c) => Prelude.Fractional (Builder v g (Value r c)) where  
  (/) = (Field./)
  recip = Field.recip
  fromRational = imm . Prelude.fromRational
  
{-

  
instance Ring.C a => Ring.C (Expr a) where
  one         = Term one
  x * y       = Expr A.Mul [x, y]
  fromInteger = Term . fromInteger
  x ^ y       = Expr Ipow [x, fromInteger y]
  

-}