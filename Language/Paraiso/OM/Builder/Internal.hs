{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, RankNTypes, TypeSynonymInstances  #-}
{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
-- Builder is only for Graph vector gauge () . 
-- Graphs with other annotation types can be created by fmap.
-- This module exports everything, for writing other Builder modules.

module Language.Paraiso.OM.Builder.Internal
    (
     Builder, BuilderState(..),
     B, BuilderOf,
     initState,
     modifyG, getG, freeNode, addNode, valueToNode, lookUpStatic,
     load, store,
     reduce, broadcast,
     shift, loadIndex,
     imm, mkOp1, mkOp2
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
import Language.Paraiso.OM.Reduce as Reduce
import Language.Paraiso.OM.Value as Val
import Language.Paraiso.Prelude
import Language.Paraiso.Tensor
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
type BuilderOf r c =  (Vector v, Ring.C g) => Builder v g (Value r c)

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


-- | convert a 'Value' to a 
valueToNode :: (TRealm r, Typeable c) => Value r c -> B FGL.Node
valueToNode val = do
  let 
      con = Val.content val
      type0 = toDyn val
  case val of
    FromNode _ _ n -> return n
    FromImm _ _ -> do
             n0 <- addNode [] (NInst (Imm (typeOf con) (Dynamic.toDyn con)) ())
             n1 <- addNode [n0] (NValue type0 ())
             return n1

-- | look up the 'NamedValue' with the correct name and type 
-- is included in the 'staticValues' of the 'BuilderState'
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

-- | Load from a static value.
load :: (TRealm r, Typeable c) => 
        r             -- ^The 'TRealm' type.
     -> c             -- ^The 'Val.content' type.
     -> Name          -- ^The 'Name' of the static value to load.
     -> B (Value r c) -- ^The result.
load r0 c0 name0 = do
  let 
      type0 = mkDyn r0 c0
      nv = NamedValue name0 type0
  lookUpStatic nv
  n0 <- addNode [] (NInst (Load name0) ())
  n1 <- addNode [n0] (NValue type0 ())
  return (FromNode r0 c0 n1)

-- | Store to a static value.
store :: (Vector v, Ring.C g, TRealm r, Typeable c) => 
         Name                    -- ^The 'Name' of the static value to store.
      -> Builder v g (Value r c) -- ^The 'Value' to be stored.
      -> Builder v g ()          -- ^The result.
store name0 builder0 = do
  val0 <- builder0
  let 
      type0 = toDyn val0
      nv = NamedValue name0 type0
  lookUpStatic nv
  n0 <- valueToNode val0
  _ <- addNode [n0] (NInst (Store name0) ())
  return ()


-- | Reduce over a 'TLocal' 'Value' 
-- using the specified reduction 'Reduce.Operator'
-- to make a 'TGlobal' 'Value'
reduce :: (Vector v, Ring.C g, Typeable c) => 
          Reduce.Operator               -- ^The reduction 'Reduce.Operator'.
       -> Builder v g (Value TLocal c)  -- ^The 'Value' to be reduced.
       -> Builder v g (Value TGlobal c) -- ^The result.
reduce op builder1 = do 
  val1 <- builder1
  let 
      c1 = Val.content val1
      type2 = mkDyn TGlobal c1
  n1 <- valueToNode val1
  n2 <- addNode [n1] (NInst (Reduce op) ())
  n3 <- addNode [n2] (NValue type2 ())
  return (FromNode TGlobal c1 n3)

-- | Broadcast a 'TGlobal' 'Value' 
-- to make it a 'TLocal' 'Value'  
broadcast :: (Vector v, Ring.C g, Typeable c) => 
             Builder v g (Value TGlobal c) -- ^The 'Value' to be broadcasted.
          -> Builder v g (Value TLocal c)  -- ^The result.
broadcast builder1 = do 
  val1 <- builder1
  let 
      c1 = Val.content val1
      type2 = mkDyn TLocal c1
  n1 <- valueToNode val1
  n2 <- addNode [n1] (NInst Broadcast ())
  n3 <- addNode [n2] (NValue type2 ())
  return (FromNode TLocal c1 n3)
  
-- | shift the orthotope with a constant vector
shift :: (Vector v, Ring.C g, Typeable c) => 
         v g                          -- ^ The amount of shift  
      -> Builder v g (Value TLocal c) -- ^ The 'Local' Value to be shifted
      -> Builder v g (Value TLocal c) -- ^ The result
shift vec builder1 = do
  val1 <- builder1
  let 
    type1 = toDyn val1
    c1 = Val.content val1
  n1 <- valueToNode val1
  n2 <- addNode [n1] (NInst (Shift vec) ())
  n3 <- addNode [n2] (NValue type1 ())
  return (FromNode TLocal c1 n3)

-- | load the mesh index 
loadIndex :: (Vector v, Ring.C g, Typeable c) => 
             c                            -- ^The 'Val.content' type.
          -> Axis v                       -- ^ The axis for which index is required
          -> Builder v g (Value TLocal c) -- ^ The result
loadIndex c0 axis = do
  let type0 = mkDyn TLocal c0
  n0 <- addNode [] (NInst (LoadIndex axis) ())
  n1 <- addNode [n0] (NValue type0 ())
  return (FromNode TLocal c0 n1)


-- | Create an immediate 'Value' from a concrete value. 'TRealm' is type-inferred.
imm :: (TRealm r, Typeable c) => c -> B (Value r c)
imm c0 = return (FromImm unitTRealm c0)

-- | Make a unary operator
mkOp1 :: (Vector v, Ring.C g, TRealm r, Typeable c) => 
         A.Operator                -- ^The operator symbol
      -> (Builder v g (Value r c)) -- ^Input
      -> (Builder v g (Value r c)) -- ^Output              
mkOp1 op builder1 = do
  v1 <- builder1
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n0 <- addNode [n1] (NInst (Arith op) ())
  n01 <- addNode [n0] (NValue (toDyn v1) ())
  return $ FromNode r1 c1 n01

-- | Make a binary operator
mkOp2 :: (Vector v, Ring.C g, TRealm r, Typeable c) => 
         A.Operator                -- ^The operator symbol 
      -> (Builder v g (Value r c)) -- ^Input 1              
      -> (Builder v g (Value r c)) -- ^Input 2               
      -> (Builder v g (Value r c)) -- ^Output              
mkOp2 op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <- addNode [n1, n2] (NInst (Arith op) ())
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

instance (Vector v, Ring.C g, TRealm r) => Boolean (Builder v g (Value r Bool)) where  
  true  = imm True
  false = imm False
  not   = mkOp1 A.Not
  (&&)  = mkOp2 A.And
  (||)  = mkOp2 A.Or
