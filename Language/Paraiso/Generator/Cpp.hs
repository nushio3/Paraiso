{-# LANGUAGE FlexibleContexts, FlexibleInstances, 
  MultiParamTypeClasses, NoImplicitPrelude,
  TypeFamilies #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..), autoStrategy
    ) where
import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Data.Dynamic
import qualified Data.Graph.Inductive as FGL
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, listToMaybe)
import           Language.Paraiso.Failure
import           Language.Paraiso.Generator
import qualified Language.Paraiso.Generator.Allocation as Alloc
import           Language.Paraiso.OM.Arithmetic (arity)
import qualified Language.Paraiso.OM.Arithmetic as A
import           Language.Paraiso.OM.DynValue as DVal
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.OM.Realm (Realm(..))
import qualified Language.Paraiso.OM.Reduce as Reduce
import           Language.Paraiso.Prelude
import           Language.Paraiso.POM as POM
import           Language.Paraiso.Tensor
import           System.Directory
import           System.FilePath
import           Unsafe.Coerce

-- | The c++ code generator.
data Cpp = Cpp deriving (Eq, Show)

autoStrategy :: Strategy Cpp
autoStrategy = CppStrategy Alloc.Auto

instance Generator Cpp where
  data Strategy Cpp = CppStrategy { 
    allocStrategy :: Alloc.Allocation 
                       } deriving (Eq, Show)
                                  
  generate _ pom0 path = do
    let 
      pom1 = decideStrategy pom0
      members = makeMembers pom1
      headerFn = nameStr pom1 ++ ".hpp"
      cppFn = nameStr pom1 ++ ".cpp"
    createDirectoryIfMissing True path
    writeFile "output/POM1.txt" $ show $ (unsafeCoerce pom1 :: POM Vec2 Int (Strategy Cpp))
    writeFile (path </> headerFn) $ genHeader members pom1
    writeFile (path </> cppFn) $ genCpp headerFn members pom1


{----                                                                -----}
{---- Translations of names, symbols, types and values               -----}
{----                                                                -----}

instance Symbolable Cpp Int where
  symbolF Cpp x = return (show x)

instance Symbolable Cpp Dynamic where
  symbolF Cpp dyn = let
    ret = msum $ map ($dyn) dynamicDB
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate symbol of type: " ++ show dyn
  
instance Symbolable Cpp TypeRep where
  symbolF Cpp tr = let
    ret = msum $ map ($tr) typeRepDB
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate type: " ++ show tr
  

instance Symbolable Cpp DVal.DynValue where
  symbolF Cpp dyn0 = do
    let
      container :: String -> String
      container = case DVal.realm dyn0 of
                    Global -> id
                    Local -> ("std::vector<"++).(++">")
    type0 <- symbolF Cpp $ DVal.typeRep dyn0
    return $ container type0

instance Symbolable Cpp Name where
  symbolF Cpp = return . nameStr
  
-- | The databeses for Haskell -> Cpp immediate values translations.
dynamicDB:: [Dynamic -> Maybe String]
dynamicDB = map fst symbolDB

-- | The databeses for Haskell -> Cpp type name translations.
typeRepDB:: [TypeRep -> Maybe String]
typeRepDB = map snd symbolDB

symbolDB:: [(Dynamic -> Maybe String, TypeRep -> Maybe String)]
symbolDB = [ 
     add "bool" (\x->if x then "true" else "false"),
     add "int" (show::Int->String), 
     add "long long int" (show::Integer->String), 
     add "float" ((++"f").show::Float->String), 
     add "double" (show::Double->String)
          ]  
  where
    add ::  (Typeable a) => String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add = add' undefined
    add' :: (Typeable a) => a -> String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add' dummy typename f = 
      (fmap f . fromDynamic, 
       \tr -> if tr==typeOf dummy then Just typename else Nothing)

{----                                                                -----}
{---- Make decisions on code generation strategies                   -----}
{----                                                                -----}

decideStrategy :: (Vector v, Ring.C g) => 
                  POM v g (Strategy Cpp)
               -> POM v g (Strategy Cpp)
decideStrategy = POM.mapGraph dSGraph
  where
    dSGraph :: (Vector v, Ring.C g) => 
               Graph v g (Strategy Cpp)
            -> Graph v g (Strategy Cpp)
    dSGraph graph = FGL.gmap 
      (\(pre,n,lab,suc) -> (pre,n,fmap (modify graph n) lab,suc)) graph

    modify :: (Vector v, Ring.C g) => 
              Graph v g (Strategy Cpp) 
           -> FGL.Node
           -> Strategy Cpp
           -> Strategy Cpp
    modify graph n (CppStrategy alloc) = CppStrategy alloc'
      where
        alloc' = if alloc /= Alloc.Auto 
                 then alloc
                 else decideAlloc graph n
    decideAlloc :: (Vector v, Ring.C g) => 
                   Graph v g (Strategy Cpp) 
                -> FGL.Node
                -> Alloc.Allocation
    decideAlloc graph n = 
      if isGlobal || afterLoad || isStore || beforeReduce || afterReduce
      then Alloc.Manifest
      else Alloc.Delayed
        where
          self0 = FGL.lab graph n
          pre0  = FGL.lab graph =<<(listToMaybe $ FGL.pre graph n) 
          suc0  = FGL.lab graph =<<(listToMaybe $ FGL.suc graph n) 
          isGlobal  = case self0 of
                        Just (NValue (DVal.DynValue Global _) _) -> True
                        _                                        -> False
          afterLoad = case pre0 of
                        Just (NInst (Load _) _) -> True
                        _                       -> False
          isStore   = case self0 of
                        Just (NInst (Store _) _) -> True
                        _                        -> False
          beforeReduce = case suc0 of
                        Just (NInst (Reduce _) _) -> True
                        _                         -> False
          afterReduce = case pre0 of
                        Just (NInst (Reduce _) _) -> True
                        _                         -> False


{----                                                                -----}
{---- c++ class header generation                                    -----}
{----                                                                -----}

-- | Access type of c++ class members
data AccessType = ReadWrite | ReadInit | ReadDepend String

data CMember = CMember {accessType :: AccessType, memberDV :: (Named DynValue)}

instance Nameable CMember where
  name = name . memberDV


sizeName :: Name
sizeName = Name "size"
sizeNameCall :: String
sizeNameCall = (++"()") . nameStr $ sizeName

sizeForAxis :: (Vector v) => Axis v -> Name
sizeForAxis axis = Name $ "size" ++ show (axisIndex axis)
sizeForAxisCall :: (Vector v) => Axis v -> String
sizeForAxisCall = (++"()") . nameStr . sizeForAxis
   


            
fglNodeName :: FGL.Node -> Name    
fglNodeName n = Name $ "a" ++ show n


makeMembers :: (Vector v, Ring.C g) => POM v g a -> [CMember]
makeMembers pom =  [sizeMember] ++ sizeAMembers ++ map (CMember ReadWrite) vals 
  where
    vals = staticValues $ POM.setup pom

    f :: (Vector v, Ring.C g) => POM v g a -> v CMember
    f _ = compose (\axis -> CMember ReadInit (Named (sizeForAxis axis) globalInt))

    sizeMember :: CMember
    sizeMember = CMember (ReadDepend $ "return " ++ prod ++ ";") (Named sizeName globalInt)
    globalInt = DynValue Global (typeOf (undefined::Int))

    sizeAMembers :: [CMember]
    sizeAMembers = foldMap (:[]) $ f pom
    
    prod :: String
    prod = concat $ List.intersperse " * " $ map (\m -> nameStr m ++ "()") sizeAMembers

genHeader :: (Vector v, Ring.C g) => [CMember] -> POM v g a -> String
genHeader members pom = unlines[
  commonInclude ,
  "class " ++ nameStr pom ++ "{",
  decStr,
  readerStr,
  writerStr,
  "public:",
  constructorStr,
  kernelStr,
  "};"
                ]
  where
    declare (Named name0 dyn0) =
      symbol Cpp dyn0 ++ " " ++ symbol Cpp name0 ++ "_;"
    decStr = unlines $ ("private:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadDepend _ -> []
                            _            -> [declare dv])

    reader (ref',code) (Named name0 dyn0) =
      let name1 = symbol Cpp name0 in
      "const " ++ symbol Cpp dyn0 ++ " " ++ ref' ++ name1 ++ "() const { " ++ code name1 ++" }"
    readerCode n = "return " ++ n ++ "_ ;"
    readerStr = unlines $ ("public:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadDepend s -> [reader ("" ,const s)    dv]
                            _            -> [reader ("&",readerCode) dv])

    writer (ref',code) (Named name0 dyn0) =
      let name1 = symbol Cpp name0 in
      symbol Cpp dyn0 ++ " " ++ ref' ++ name1 ++ "() { " ++ code name1 ++" }"
    writerCode n = "return " ++ n ++ "_ ;"
    writerStr = unlines $ ("public:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadWrite -> [writer ("&" ,writerCode) dv]
                            _         -> [])

    initializer (Named name0 _) = let name1 = symbol Cpp name0 in
      name1 ++ "_(" ++ name1 ++ ")"
    initializeIfLocal (Named name0 dyn0) = let name1 = symbol Cpp name0 in
      if DVal.realm dyn0 == Global
      then []
      else [name1 ++ "_(" ++ symbol Cpp sizeName ++ "())"]
    initializerStr = concat $ List.intersperse "," $ concat $
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadInit  -> [initializer dv]
                            ReadWrite -> initializeIfLocal dv
                            _         -> [])
    cArg (Named name0 dyn0) = let name1 = symbol Cpp name0 in
      symbol Cpp dyn0 ++ " " ++ name1
    cArgStr = concat $ List.intersperse "," $ concat $
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadInit -> [cArg dv]
                            _        -> [])
    constructorStr = nameStr pom ++ " ( " ++ cArgStr ++ " ): " 
                     ++ initializerStr ++ "{}"
    
    kernelStr = unlines $ map (\kernel -> "void " ++ nameStr kernel ++ " ();") $
                kernels pom


commonInclude :: String
commonInclude = unlines[
                 "#include <vector>",
                 "#include <cmath>",
                 ""
                ]

{----                                                                -----}
{---- c++ kernel generating tools                                    -----}
{----                                                                -----}

-- | A representation for Addressed Single Static Assignment.
data Cursor v g = 
  -- | node number and shift
  CurLocal  { cursorToFGLNode :: FGL.Node, cursorToShift :: (v g)} |
  -- | node number 
  CurGlobal { cursorToFGLNode :: FGL.Node }
              deriving (Eq, Ord)

instance Show (Cursor v g) where
  show (CurLocal n _) = "/*L " ++ show n ++ "*/"
  show (CurGlobal n ) = "/*G " ++ show n ++ "*/"
  
                       
data Context  = 
    CtxGlobal |
    CtxLocal  Name     -- ^The name of the indexing variable.
    deriving (Eq, Ord, Show)

type BindingMap v g= Map (Cursor v g) String
data BinderState v g = BinderState {  
  context     :: Context,
  graphCtx    :: Graph v g (Strategy Cpp),
  bindings    :: BindingMap v g
    }              deriving (Show)

type Binder v g a = State (BinderState v g) a
 
data HandSide = LeftHand | RightHand deriving (Eq, Show)

paren :: String -> String
paren x =  "(" ++ x ++ ")"

arithRep :: A.Operator -> [String] -> String
arithRep op = let
    unary symb [x] = paren $ unwords [symb,x]
    infx symb [x,y] = paren $ unwords [x,symb,y]
    func symb xs = symb ++ paren (List.concat $ List.intersperse "," xs)
    err = error $ "undefined operator : " ++ show op
  in case op of
    A.Add -> infx "+"
    A.Sub -> infx "-"
    A.Neg -> unary "-"
    A.Mul -> infx "*" 
    A.Div -> infx "/" 
    A.Inv -> unary "1/"
    A.Not -> unary "!"
    A.And -> infx "&&" 
    A.Or -> infx "||" 
    A.EQ -> infx "==" 
    A.NE -> infx "!=" 
    A.LT -> infx "<" 
    A.LE -> infx "<=" 
    A.GT -> infx ">" 
    A.GE -> infx ">=" 
    A.Select -> (\[x,y,z] -> paren $ unwords [x,"?",y,":",z])
    A.Ipow -> func "pow"
    A.Pow -> func "pow"
    A.Madd -> err
    A.Msub ->  err
    A.Nmadd ->  err
    A.Nmsub ->  err
    A.Sincos ->  err
            


runBinder :: (Additive.C (v g)) =>
  Graph v g (Strategy Cpp) -> FGL.Node -> (Cursor v g -> Binder v g ()) -> String
runBinder graph0 n0 binder = unlines $ header ++  [bindStr] ++ footer
  where 
    rlm = lhsRealm graph0 n0
    bindStr = unlines $ Map.elems $ bindings state
    state = snd $ State.runState (binder iniCur) ini
    
    iniCur = case rlm of
               Global -> CurGlobal n0
               Local  -> CurLocal  n0 Additive.zero
    ini = BinderState {
            context  = case rlm of 
                         Global -> CtxGlobal
                         Local  -> CtxLocal $ Name "i",
            graphCtx = graph0,
            bindings = Map.empty
          }
    
    (header,footer) = case context state of
      CtxGlobal -> ([],[])
      CtxLocal loopIndex -> ([loop (symbol Cpp loopIndex) ++ " {"], ["}"])
    loop i =
      "for (int " ++ i ++ " = 0 ; " 
                  ++ i ++ " < " ++ symbol Cpp sizeName ++ "() ; " 
                  ++  "++" ++ i ++ ")"

lhsRealm :: Graph v g (Strategy Cpp) -> FGL.Node -> Realm 
lhsRealm graph n = 
  case fromJust $ FGL.lab graph n of     
    NValue dyn0 _ -> DVal.realm dyn0
    NInst inst  _ -> 
      case inst of
        Store _ -> lhsRealm graph $ head $ FGL.pre graph n
        _       -> undefined


bindersGraph :: Binder v g (Graph v g (Strategy Cpp))
bindersGraph =  fmap graphCtx State.get

bindersContext :: Binder v g Context
bindersContext = fmap context State.get

bindersMap :: Binder v g (BindingMap v g)
bindersMap = fmap bindings State.get


bindingModify :: (BindingMap v g -> BindingMap v g) -> Binder v g ()
bindingModify f = do
  s <- State.get
  m <- bindersMap
  State.put s{bindings = f m}

cursorToNode :: (Cursor v g) -> Binder v g (Node v g (Strategy Cpp))
cursorToNode cur = do
  graph <- bindersGraph
  return $ fromJust $ FGL.lab graph $ cursorToFGLNode cur

-- | add @cursor@ in the current binding, if missing.
addBinding :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) => Cursor v g -> Binder v g ()
addBinding cursor = do 
  graph <- bindersGraph
  m <- bindersMap
  if Map.member cursor m
     then return ()
     else do
       lhs <- leftHandSide cursor
       let
         -- any Node that has well-defined LHS must have one and only one pre Node.
         preNode = head $ FGL.pre graph(cursorToFGLNode cursor)
         preCursor = cursor{cursorToFGLNode = preNode}
       rhs <- rightHandSide preCursor
       bindingModify $ Map.insert cursor (lhs ++ " = " ++ rhs ++ ";")


cursorToSymbol :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) =>
                  HandSide
               -> Cursor v g 
               -> Binder v g String
cursorToSymbol side cur = do
  node  <- cursorToNode cur
  ctx <- bindersContext
  let axer = \(ax, shiftAmount) -> do
               idxStr <- rhsLoadIndex ax
               return (ax, idxStr)
  axes3 <- traverse axer $ compose (\ax -> (ax, cursorToShift cur!ax))
  let 
    name0 = case node of
              NValue _ _   -> fglNodeName $ cursorToFGLNode cur
              NInst inst _ -> case inst of
                                Store name1 -> Name $ (++ "()") $ nameStr name1
                                Load  name1 -> Name $ (++ "()") $ nameStr name1
                                _           -> error $ "this Inst does not have symbol" 
    typeDelayed = case node of
                    NValue dyn0 _ -> symbol Cpp dyn0{DVal.realm = Global}
                    _             -> error "no type"
    alloc = allocStrategy $ getA node 
    prefix = if side == LeftHand && alloc == Alloc.Delayed 
             then "const " ++ typeDelayed ++ " " else ""
    isManifest = case alloc of
                   Alloc.Delayed  -> case node of
                                       NValue _ _ -> False
                                       _          -> True
                   _              -> True
                     
    suffix i = if isManifest then "[" ++ shiftStr i ++ "]" 
                             else foldMap cppoku (cursorToShift cur)
    cppoku = (("_"++).(map (\c->if c=='-' then 'm' else c)).symbol Cpp)
    
    shiftStr i = if shift == Additive.zero 
               then nameStr i
               else fst (mapAccumR shiftAccum "" allAxes)
    allAxes  = fmap fst axes3
    idxAxes  = fmap snd axes3
    shift    = cursorToShift cur

    shiftedAxis ax = paren$
      (paren $ unwords [idxAxes ! ax, "+", symbol Cpp (shift ! ax),"+",sizeForAxisCall ax])
      ++ "%" ++ sizeForAxisCall ax

    shiftAccum str ax = 
      if (axisIndex ax::Int) == dimension allAxes - 1
      then (shiftedAxis ax, ())
      else (unwords [shiftedAxis ax, "+", sizeForAxisCall ax , "*", paren str], ())

  case ctx of
    CtxGlobal  -> return $ nameStr name0
    CtxLocal i -> return $ prefix ++ nameStr name0 ++ suffix i

leftHandSide :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) =>
                Cursor v g -> Binder v g String
leftHandSide = cursorToSymbol LeftHand

rightHandSide :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) =>
                 Cursor v g -> Binder v g String
rightHandSide cur = do
  node0  <- cursorToNode cur
  case node0 of
    NInst inst _ -> rhsInst inst cur
    NValue _ _   -> do 
               when (allocStrategy (getA node0) == Alloc.Delayed) $ addBinding cur 
               cursorToSymbol RightHand cur


rhsInst :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) =>
           Inst v g -> Cursor v g -> Binder v g String
rhsInst inst cursor = do
  graph <- bindersGraph
  let 
    -- FGL indices of all the preceding nodes.
    preNodes   = List.sort $ FGL.pre graph(cursorToFGLNode cursor)
    -- Cursors of all the preceding nodes with context unchanged.
    preCursors = map (\n -> cursor{cursorToFGLNode = n}) preNodes
    headCursor = head preCursors
  case inst of
    Imm dyn0    -> return $ symbol Cpp dyn0
    Load   _    -> cursorToSymbol RightHand cursor
    Store  _    -> error "Store has no RHS!"
    Reduce op   -> return "0/*Reduce madayanen*/"
    Broadcast   -> cursorToSymbol RightHand (CurGlobal $ head preNodes)
    Shift vec   -> cursorToSymbol RightHand headCursor
                   {cursorToShift = vec + cursorToShift headCursor}
    LoadIndex a -> rhsLoadIndex a
    Arith op    -> do
              xs <- mapM rightHandSide preCursors
              return $ arithRep op xs

rhsLoadIndex :: (Vector v, Symbolable Cpp g, Additive.C (v g), Ord (v g)) =>
                Axis v -> Binder v g String
rhsLoadIndex axis = do
  ctx <- bindersContext
  let
      loopVar = case ctx of
                  CtxGlobal  -> error "cannot load index in gloabl context"
                  CtxLocal i -> nameStr i
      axesSmaller = List.filter (\ax -> axisIndex ax < axisIndex axis) (allAxes axis)
      divs = paren $ unwords $ List.intersperse "/" $  loopVar : map sizeForAxisCall axesSmaller
      ret =  paren $ unwords $ [divs , "%" ,sizeForAxisCall axis]
      allAxes axis = foldMap (:[]) $ compose (\axis' -> head [axis', axis])

  return ret

{----                                                                -----}
{---- c++ kernel generation                                          -----}
{----                                                                -----}


genCpp :: (Vector v, Ring.C g, Additive.C (v g), Ord (v g),  Symbolable Cpp g) =>
          String -> [CMember] -> POM v g (Strategy Cpp) -> String
genCpp headerFn _ pom = unlines [
  "#include \"" ++ headerFn ++ "\"",
  "",
  kernelsStr
                       ]
  where
    classPrefix = nameStr pom ++ "::"
    kernelsStr = unlines $ map (declareKernel classPrefix) $
                kernels pom


declareKernel :: (Vector v, Ring.C g, Additive.C (v g), Ord (v g), Symbolable Cpp g) => 
                 String -> Kernel v g (Strategy Cpp)-> String
declareKernel classPrefix kern = unlines [
  "void " ++ classPrefix ++ nameStr kern ++ " () {",
  declareNodes labNodes,
  substituteNodes labNodes,
  "return;",
  "}"
                     ]
  where
    graph = dataflow kern
    labNodes = FGL.labNodes graph

    nodeToRealm n = case (fromJust $ FGL.lab graph n) of
      NValue dyn0 _ -> DVal.realm dyn0
      _ -> error "nodeToRealm called on NInst"

    declareNodes = unlines . concat . map declareNode
    declareNode (n, node) = case node of
        NInst _ _  -> []
        NValue dyn0 (CppStrategy Alloc.Delayed) -> []
        NValue dyn0 _ -> [declareVal (nameStr $ fglNodeName n) dyn0]
    declareVal name0 dyn0 = let
        x = if DVal.realm dyn0 == Local 
          then "(" ++ symbol Cpp sizeName ++ "())"
          else ""
      in symbol Cpp dyn0 ++ " " ++ name0 ++ x ++ ";"
    substituteNodes = unlines. concat . map substituteNode
    substituteNode (n, node) = case allocStrategy $ getA node of
                                 Alloc.Manifest -> [genSub n node]
                                 _              -> []
    genSub n node = 
      runBinder graph n addBinding

        

