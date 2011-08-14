{-# LANGUAGE FlexibleInstances, KindSignatures, NoImplicitPrelude, 
  PackageImports, RankNTypes, TypeSynonymInstances  #-}
{-# OPTIONS -Wall #-}

-- | A monadic library to build dataflow graphs for OM. 
-- Builder is only for Graph vector gauge () . 
-- Graphs with other annotation types can be created by fmap.
-- This module exports everything, for writing other Builder modules.

module Language.Paraiso.OM.Builder.Internal
    (
     Builder, BuilderState(..),
     B, BuilderOf,
     buildKernel, initState, 
     modifyG, getG, freeNode, addNode, addNodeE, valueToNode, lookUpStatic,
     load, store,
     reduce, broadcast,
     shift, loadIndex,loadSize,
     imm, mkOp1, mkOp2
    ) where
import qualified Algebra.Absolute as Absolute
import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Lattice as Lattice
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.ZeroTestable as ZeroTestable
import           Control.Monad
import qualified "mtl" Control.Monad.State as State
import qualified Data.Graph.Inductive as FGL
import           Data.Dynamic (Typeable)
import qualified Data.Dynamic as Dynamic
import qualified Data.Vector  as V
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM.Arithmetic as A
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.Realm as Realm
import           Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.OM.Value as Val
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor
import qualified Prelude (Num(..), Fractional(..))

-- | Create a 'Kernel' from a 'Builder' monad.
buildKernel :: 
              Setup v g a      -- ^The Orthotope machine setup.
           -> Name             -- ^The name of the kernel.
           -> Builder v g a () -- ^The builder monad.
           -> Kernel v g a     -- ^The created kernel.
buildKernel setup0 name0 builder0 = let
    state0 = initState setup0
    graph = target $ snd $ State.runState builder0 state0
  in Kernel{kernelName = name0, dataflow = graph}


data BuilderState vector gauge anot = BuilderState 
    { setup   :: Setup vector gauge anot, 
      context :: BuilderContext anot,
      target  :: Graph vector gauge anot} deriving (Show)

data BuilderContext anot = 
  BuilderContext 
  { currentAnnotation :: anot } deriving (Show)

-- | Create an initial state for 'Builder' monad from a OM 'Setup'.
initState :: Setup v g a -> BuilderState v g a
initState s = BuilderState {
                setup   = s,
                context = BuilderContext{currentAnnotation = globalAnnotation s},
                target  = FGL.empty
              }

-- | The 'Builder' monad is used to build 'Kernel's.
type Builder (vector :: * -> *) (gauge :: *) (anot :: *) (val :: *) = 
  State.State (BuilderState vector gauge anot) val

--  'Builder' needs to be an instance of 'Eq' to become an instance of  'Prelude.Num'  
instance Eq (Builder v g a ret) where
  _ == _ = undefined
--  'Builder' needs to be an instance of 'Show' to become an instance of  'Prelude.Num'  
instance Show (Builder v g a ret) where
  show _ = "<<REDACTED>>"

type B ret = forall (v :: * -> *) (g :: *) (a :: *). Builder v g a ret
type BuilderOf r c = forall (v :: * -> *) (g :: *) (a :: *).  Builder v g a (Value r c)

-- | Modify the dataflow graph stored in the 'Builder'.
modifyG ::           
  (Graph v g a -> Graph v g a) -- ^The graph modifying function.
  -> Builder v g a ()                             -- ^The state gets silently modified.
modifyG f = State.modify (\bs -> bs{target = f.target $ bs})

-- | Get the graph stored in the 'Builder'.
getG :: Builder v g a (Graph v g a)
getG = fmap target State.get

-- | get the number of the next unoccupied 'FGL.Node' in the graph.
freeNode :: B FGL.Node
freeNode = do
  n <- fmap (FGL.noNodes) getG
  return n

-- | add a node to the graph.
addNode :: 
           [FGL.Node]             -- ^The list of dependent nodes. The order is recorded.
           -> Node v g a -- ^The new node to be added.
           -> Builder v g a FGL.Node
addNode froms new = do
  n <- freeNode
  modifyG (([(EOrd i, froms !! i) | i <-[0..length froms - 1] ], n, new, []) FGL.&)
  return n

-- | add a node to the graph with an empty Annotation.
addNodeE :: 
           [FGL.Node]                             -- ^The list of dependent nodes. The order is recorded.
           -> (a -> Node v g a) -- ^The new node to be added, with Annotation missing.
           -> Builder v g a FGL.Node
addNodeE froms new' = do
  anot <- fmap (currentAnnotation . context) State.get
  addNode froms (new' anot)


-- | convert a 'Value' to a 
valueToNode :: (TRealm r, Typeable c) => Value r c -> B FGL.Node
valueToNode val = do
  let 
      con = Val.content val
      type0 = toDyn val
  case val of
    FromNode _ _ n -> return n
    FromImm _ _ -> do
             n0 <- addNodeE []   $ NInst (Imm (Dynamic.toDyn con))
             n1 <- addNodeE [n0] $ NValue type0
             return n1

-- | look up the 'Named' 'DynValue' with the correct name and type 
-- is included in the 'staticValues' of the 'BuilderState'
lookUpStatic :: Named DynValue -> B StaticIdx
lookUpStatic (Named name0 type0)= do
  st <- State.get 
  let
      vs :: V.Vector (Named DynValue)
      vs = staticValues $ setup st
      matches = V.filter (\(_,v)-> name v==name0) $ V.imap (\i v->(i,v)) vs
      (ret, Named _ type1) = if V.length matches /= 1 
                             then error (show (V.length matches)++" match found for '" ++ nameStr name0 ++ 
                                         "' in " ++ show vs)
                             else V.head matches  
  when (type0 /= type1) $ error ("type mismatch; expected: " ++ show type1 ++ "; " ++
                                " actual: " ++ nameStr name0 ++ "::" ++ show type0)
  return $ StaticIdx ret

-- | Load from a static value.
load :: (TRealm r, Typeable c) => 
        r             -- ^The 'TRealm' type.
     -> c             -- ^The 'Val.content' type.
     -> Name          -- ^The 'Name' of the static value to load.
     -> B (Value r c) -- ^The loaded 'TLocal' 'Value' as a result.
load r0 c0 name0 = do
  let 
      type0 = mkDyn r0 c0
      nv = Named name0 type0
  idx <- lookUpStatic nv
  n0 <- addNodeE []   $ NInst  (Load idx)
  n1 <- addNodeE [n0] $ NValue type0 
  return (FromNode r0 c0 n1)

-- | Store to a static value.
store :: (TRealm r, Typeable c) => 
         Name                    -- ^The 'Name' of the static value to store.
      -> Builder v g a (Value r c) -- ^The 'Value' to be stored.
      -> Builder v g a ()          -- ^The result.
store name0 builder0 = do
  val0 <- builder0
  let 
      type0 = toDyn val0
      nv = Named name0 type0
  idx <- lookUpStatic nv
  n0 <- valueToNode val0
  _ <- addNodeE [n0] $ NInst (Store idx) 
  return ()


-- | Reduce over a 'TLocal' 'Value' 
-- using the specified reduction 'Reduce.Operator'
-- to make a 'TGlobal' 'Value'
reduce :: (Typeable c) => 
          Reduce.Operator               -- ^The reduction 'Reduce.Operator'.
       -> Builder v g a (Value TLocal c)  -- ^The 'TLocal' 'Value' to be reduced.
       -> Builder v g a (Value TGlobal c) -- ^The 'TGlobal' 'Value' that holds the reduction result.
reduce op builder1 = do 
  val1 <- builder1
  let 
      c1 = Val.content val1
      type2 = mkDyn TGlobal c1
  n1 <- valueToNode val1
  n2 <- addNodeE [n1] $ NInst (Reduce op) 
  n3 <- addNodeE [n2] $ NValue type2 
  return (FromNode TGlobal c1 n3)

-- | Broadcast a 'TGlobal' 'Value' 
-- to make it a 'TLocal' 'Value'  
broadcast :: (Typeable c) => 
             Builder v g a (Value TGlobal c) -- ^The 'TGlobal' 'Value' to be broadcasted.
          -> Builder v g a (Value TLocal c)  -- ^The 'TLocal' 'Value', all of them containing the global value.
broadcast builder1 = do 
  val1 <- builder1
  let 
      c1 = Val.content val1
      type2 = mkDyn TLocal c1
  n1 <- valueToNode val1
  n2 <- addNodeE [n1] $ NInst Broadcast 
  n3 <- addNodeE [n2] $ NValue type2 
  return (FromNode TLocal c1 n3)

-- | Shift a 'TLocal' 'Value' with a constant vector.
shift :: (Typeable c)
  => v g                            -- ^ The amount of shift  
  -> Builder v g a (Value TLocal c) -- ^ The 'TLocal' Value to be shifted
  -> Builder v g a (Value TLocal c) -- ^ The shifted 'TLocal' 'Value' as a result.
shift vec builder1 = do
  val1 <- builder1
  let 
    type1 = toDyn val1
    c1 = Val.content val1
  n1 <- valueToNode val1
  n2 <- addNodeE [n1] $ NInst $ Shift vec
  n3 <- addNodeE [n2] $ NValue type1 
  return (FromNode TLocal c1 n3)

-- | Load the 'Axis' component of the mesh address, to a 'TLocal' 'Value'.
loadIndex :: (Typeable c) => 
             c                            -- ^The 'Val.content' type.
          -> Axis v                       -- ^ The axis for which index is required
          -> Builder v g a (Value TLocal c) -- ^ The 'TLocal' 'Value' that contains the address as a result.
loadIndex c0 axis = do
  let type0 = mkDyn TLocal c0
  n0 <- addNodeE []   $ NInst (LoadIndex axis)
  n1 <- addNodeE [n0] $ NValue type0 
  return (FromNode TLocal c0 n1)

-- | Load the 'Axis' component of the mesh size, to a 'TGlobal' 'Value'.
loadSize :: (Typeable c)
            => c                            -- ^The 'Val.content' type.
            -> Axis v                       -- ^ The axis for which the size is required
            -> Builder v g a (Value TGlobal c) -- ^ The 'TGlobal' 'Value' that contains the size of the mesh in that direction.
loadSize c0 axis = do
  let type0 = mkDyn TLocal c0
  n0 <- addNodeE []   $ NInst (LoadSize axis)
  n1 <- addNodeE [n0] $ NValue type0 
  return (FromNode TGlobal c0 n1)


-- | Create an immediate 'Value' from a Haskell concrete value. 
-- 'TRealm' is type-inferred.
imm :: (TRealm r, Typeable c) => 
       c             -- ^A Haskell value of type @c@ to be stored.
    -> B (Value r c) -- ^'TLocal' 'Value' with the @c@ stored.
imm c0 = return (FromImm unitTRealm c0)

-- | Make a unary operator
mkOp1 :: (TRealm r, Typeable c) => 
         A.Operator                -- ^The operator symbol
      -> (Builder v g a (Value r c)) -- ^Input
      -> (Builder v g a (Value r c)) -- ^Output              
mkOp1 op builder1 = do
  v1 <- builder1
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n0 <-  addNodeE [n1] $ NInst (Arith op) 
  n01 <- addNodeE [n0] $ NValue (toDyn v1) 
  return $ FromNode r1 c1 n01

-- | Make a binary operator
mkOp2 :: (TRealm r, Typeable c) => 
         A.Operator                -- ^The operator symbol 
      -> (Builder v g a (Value r c)) -- ^Input 1              
      -> (Builder v g a (Value r c)) -- ^Input 2               
      -> (Builder v g a (Value r c)) -- ^Output              
mkOp2 op builder1 builder2 = do
  v1 <- builder1
  v2 <- builder2
  let 
      r1 = Val.realm v1
      c1 = Val.content v1
  n1 <- valueToNode v1
  n2 <- valueToNode v2
  n0 <-  addNodeE [n1, n2] $ NInst (Arith op)
  n01 <- addNodeE [n0] $ NValue (toDyn v1) 
  return $ FromNode r1 c1 n01


-- | Builder is Additive 'Additive.C'.
-- You can use 'Additive.zero', 'Additive.+', 'Additive.-', 'Additive.negate'.
instance (TRealm r, Typeable c, Additive.C c) => Additive.C (Builder v g a (Value r c)) where
  zero = return $ FromImm unitTRealm Additive.zero
  (+) = mkOp2 A.Add
  (-) = mkOp2 A.Sub
  negate = mkOp1 A.Neg

-- | Builder is Ring 'Ring.C'.
-- You can use 'Ring.one', 'Ring.*'.
instance (TRealm r, Typeable c, Ring.C c) => Ring.C (Builder v g a (Value r c)) where
  one = return $ FromImm unitTRealm Ring.one
  (*) = mkOp2 A.Mul
  fromInteger = imm . fromInteger

-- | you can convert GHC numeric immediates to 'Builder'.
instance (TRealm r, Typeable c, Ring.C c) => Prelude.Num (Builder v g a (Value r c)) where  
  (+) = (Additive.+)
  (*) = (Ring.*)
  (-) = (Additive.-)
  negate = Additive.negate
  abs = undefined
  signum = undefined
  fromInteger = Ring.fromInteger

-- | Builder is Field 'Field.C'. You can use 'Field./', 'Field.recip'.
instance (TRealm r, Typeable c, Field.C c) => Field.C (Builder v g a (Value r c)) where
  (/) = mkOp2 A.Div
  recip = mkOp1 A.Inv
  fromRational' = imm . fromRational'

-- | you can convert GHC floating point immediates to 'Builder'.
instance (TRealm r, Typeable c, Field.C c, Prelude.Fractional c) => Prelude.Fractional (Builder v g a (Value r c)) where  
  (/) = (Field./)
  recip = Field.recip
  fromRational = imm . Prelude.fromRational

-- | Builder is 'Boolean'. You can use 'true', 'false', 'not', '&&', '||'.
instance (TRealm r) => Boolean (Builder v g a (Value r Bool)) where  
  true  = imm True
  false = imm False
  not   = mkOp1 A.Not
  (&&)  = mkOp2 A.And
  (||)  = mkOp2 A.Or

-- | Builder is Algebraic 'Algebraic.C'. You can use 'Algebraic.sqrt' and so on.
instance (TRealm r, Typeable c, Algebraic.C c) => Algebraic.C (Builder v g a (Value r c)) where
  sqrt = mkOp1 A.Sqrt
  x ^/ y = mkOp2 A.Pow x (fromRational' y)

-- | choose the larger or the smaller of the two.
instance (TRealm r, Typeable c) => Lattice.C (Builder v g a (Value r c))
    where
      up = mkOp2 A.Max  
      dn = mkOp2 A.Min

instance (TRealm r, Typeable c) => ZeroTestable.C (Builder v g a (Value r c))
    where
      isZero _ = error "isZero undefined for builder."

instance (TRealm r, Typeable c, Ring.C c) => Absolute.C (Builder v g a (Value r c))
    where
      abs    = mkOp1 A.Abs
      signum = mkOp1 A.Signum

instance (TRealm r, Typeable c,  Transcendental.C c) =>  
    Transcendental.C (Builder v g a (Value r c)) where      
        pi = imm pi
        exp = mkOp1 A.Exp
        log = mkOp1 A.Log
        sin = mkOp1 A.Sin
        cos = mkOp1 A.Cos
        tan = mkOp1 A.Tan
        asin = mkOp1 A.Asin
        acos = mkOp1 A.Acos
        atan = mkOp1 A.Atan
