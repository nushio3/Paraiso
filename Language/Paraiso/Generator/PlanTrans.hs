{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, NoImplicitPrelude, 
OverloadedStrings, TupleSections #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate
  ) where


import qualified Algebra.Additive                    as Additive
import           Data.Char
import           Data.Dynamic
import qualified Data.Graph.Inductive                as FGL
import           Data.List (sortBy)
import qualified Data.ListLike.String                as LL
import           Data.ListLike.Text ()
import           Data.Maybe
import qualified Data.Set                            as Set
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import qualified Language.Paraiso.Annotation         as Anot
import qualified Language.Paraiso.Generator.Claris   as C
import qualified Language.Paraiso.Generator.Native   as Native
import qualified Language.Paraiso.Generator.Plan     as Plan
import qualified Language.Paraiso.OM.Arithmetic      as Arith
import qualified Language.Paraiso.OM.DynValue        as DVal
import qualified Language.Paraiso.OM.Graph           as OM
import qualified Language.Paraiso.OM.Realm           as Realm
import qualified Language.Paraiso.OM.Reduce           as ReduceOp
import qualified Language.Paraiso.Optimization.Graph as Opt
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

type AnAn = Anot.Annotation
data Env v g = Env (Native.Setup v g) (Plan.Plan v g AnAn)

translate :: Opt.Ready v g => Native.Setup v g -> Plan.Plan v g AnAn -> C.Program
translate setup plan = 
  C.Program 
  { C.progName = name plan,
    C.topLevel = 
      map include stlHeaders ++ 
      library ++ 
      comments ++
      [ C.ClassDef $ C.Class (name plan) $
        storageVars ++ subKernelFuncs ++ memberFuncs 
      ]
  }
  where
    env = Env setup plan
    comments = (:[]) $ C.Comment $ LL.unlines [ 
      "",
      "lowerMargin = " ++ showT (Plan.lowerMargin plan),
      "upperMargin = " ++ showT (Plan.upperMargin plan)
      ]

    memberFuncs = V.toList $ V.imap (\idx ker -> makeFunc env idx ker) $ Plan.kernels plan

    subKernelFuncs = V.toList $ V.map (makeSubFunc env) $ Plan.subKernels plan


    include = C.Exclusive C.HeaderFile . C.StmtPrpr . C.PrprInclude C.Chevron
    stlHeaders = case Native.language setup of
      Native.CPlusPlus -> ["algorithm", "cmath", "vector"]
      Native.CUDA      -> ["thrust/device_vector.h", "thrust/host_vector.h"]

    storageVars = 
      V.toList $
      V.map storageRefToMenber $
      Plan.storages plan
    storageRefToMenber stRef =  
      C.MemberVar  C.Private $ 
      C.Var 
        (mkCtyp env $ Plan.storageType stRef) 
        (name stRef) 


makeFunc :: Opt.Ready v g => Env v g -> Int -> OM.Kernel v g AnAn -> C.MemberDef
makeFunc env@(Env setup plan) kerIdx ker = C.MemberFunc C.Public $ 
 (C.function tVoid (name ker)) 
 { C.funcBody = kernelCalls ++ storeInsts
 }
 where
   graph = OM.dataflow ker

   kernelCalls = 
     V.toList $
     V.map (\subker -> callSubKer subker $ V.map findVar $ 
                       Plan.inputIdxs subker V.++ Plan.outputIdxs subker) $
     V.filter ((== kerIdx) . Plan.kernelIdx) $
     Plan.subKernels plan
   callSubKer subker xs = 
     C.StmtExpr $
     C.FuncCallUsr (name subker) (V.toList xs)

   storeInsts = 
     map swapStmt $
     concatMap (\(idx, nd) -> case nd of
               OM.NInst (OM.Store (OM.StaticIdx statIdx))_ -> [(idx, statIdx)] 
               _ -> []) $
     FGL.labNodes graph

   swapStmt (idx, statIdx) = 
     let preIdx = head $ FGL.pre graph idx in
     case (filter ((Plan.StaticRef statIdx==) . Plan.storageIdx) $ V.toList $ Plan.storages plan,
           filter ((Plan.ManifestRef kerIdx preIdx==) . Plan.storageIdx) $ V.toList $ Plan.storages plan) of
       ([stRef],[maRef]) -> 
         C.StmtExpr $ C.Op2Infix "="
         (C.VarExpr $ C.Var (mkCtyp env $ Plan.storageType stRef) (name stRef) )
         (C.VarExpr $ C.Var C.UnknownType (name maRef) )
       _ -> error $ "mismatch in storage phase: " ++ show (idx, statIdx) 
   findVar idx = 
     let 
       loadIdx = 
         listToMaybe $
         concat $
         map (\jdx -> 
               case FGL.lab graph jdx of
                 Just (OM.NInst (OM.Load (OM.StaticIdx statIdx))_)-> [Plan.StaticRef statIdx] 
                 _                                                -> []) $
         FGL.pre graph idx
       match stIdx
         | stIdx == Plan.ManifestRef kerIdx idx = True
         | Just stIdx == loadIdx                = True
         | otherwise                            = False
       stRef = V.head $ V.filter ( match . Plan.storageIdx ) $ Plan.storages plan
     in C.VarExpr $ C.Var 
        (mkCtyp env $ Plan.storageType stRef) 
        (name stRef) 


-- | Create a subKernel: a function that performs a portion of actual calculations.
makeSubFunc :: Opt.Ready v g => Env v g -> Plan.SubKernelRef v g AnAn -> C.MemberDef
makeSubFunc env subker = 
  C.MemberFunc C.Public $ 
  (C.function tVoid (name subker))
  { C.funcArgs = 
     makeSubArg env True (Plan.labNodesIn subker) ++
     makeSubArg env False (Plan.labNodesOut subker),
    C.funcBody = let r = Realm.realm subker in
      if r == Realm.Global 
      then loopMaker env r subker
      else
        [ C.Comment $ LL.unlines 
          [ "",
            "lowerMargin = " ++ showT (Plan.lowerBoundary subker),
            "upperMargin = " ++ showT (Plan.upperBoundary subker)
          ]
        ] ++ loopMaker env r subker
  }

-- | make a subroutine argument list.
makeSubArg :: Opt.Ready v g => Env v g -> Bool -> V.Vector (FGL.LNode (OM.Node v g AnAn)) -> [C.Var]
makeSubArg env isConst lnodes =
  let f = (if isConst then C.Const else id) . C.RefOf
  in
  map (\(idx,nd)-> case nd of
            OM.NValue typ _ -> C.Var (f $ mkCtyp env typ) (nodeNameUniversal idx)
            _ -> error "NValue expected" ) $
  V.toList lnodes



-- | implement the loop for each subroutine
loopMaker :: Opt.Ready v g => Env v g -> Realm.Realm -> Plan.SubKernelRef v g AnAn -> [C.Statement]
loopMaker env@(Env setup plan) realm subker = case realm of
  Realm.Local ->
    [ C.StmtFor 
      (C.VarDefSub loopCounter (intImm 0)) 
      (C.Op2Infix "<" (C.VarExpr loopCounter) (C.toDyn (product boundarySize)))
      (C.Op1Prefix "++" (C.VarExpr loopCounter)) $
      [C.VarDefSub addrCounter codecAddr] ++
      loopContent
    ]
  Realm.Global -> loopContent

  where
    loopCounter = C.Var tSizet (mkName "i")
    memorySize   = toList $ Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan
    boundarySize = toList $ Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan
     - Plan.lowerBoundary subker - Plan.upperBoundary subker

    codecDiv = 
      [ if idx == 0 then (C.VarExpr loopCounter) else C.Op2Infix "/" (C.VarExpr loopCounter) (C.toDyn $ product $ take idx boundarySize) 
      | idx <- [0..length boundarySize-1]]
    codecMod = 
      [ if idx == length codecDiv-1 then x else C.Op2Infix "%" x (C.toDyn $ boundarySize !! idx)
      | (idx, x) <- zip [0..] codecDiv]
    codecAddr = 
      if memorySize == boundarySize 
      then C.VarExpr loopCounter
      else foldl1 (C.Op2Infix "+")
           [ C.Op2Infix "*" x (C.toDyn $ memorySize !! idx)
           | (idx, x) <- zip [0..] codecMod]
    codecLoadIndex =
      [ C.Op2Infix "-" x (C.toDyn  ((Plan.lowerMargin plan - Plan.lowerBoundary subker) ! (Axis idx) ))
      | (idx, x) <- zip [0..] codecMod]
    codecLoadSize =
      [ C.toDyn  (Native.localSize setup ! (Axis idx) )
      | (idx, _) <- zip [0..] codecMod]
    codecCursor cursor = 
      (C.Op2Infix "+" (C.VarExpr addrCounter) (C.toDyn summa))
      where
        summa = sum $
          [ cursor ! (Axis idx) * product (take idx memorySize)
          | (idx, _) <- zip [0..] memorySize]


    addrCounter = C.Var tSizet (mkName "addr_origin")

    loopContent = 
      concat $
      map buildExprs $
      filterVal $
      Set.toList allIdxSet

    buildExprs (idx, val@(DVal.DynValue r c)) = 
      map (\cursor -> 
            lhs cursor
            (fst $ rhsAndRequest env idx cursor)
          ) $
      Set.toList $ lhsCursors V.! idx
      where
        lhs cursor expr = 
          if Set.member idx outputIdxSet
          then C.StmtExpr $ flip (C.Op2Infix "=") expr $ case realm of
            Realm.Local ->
              (C.ArrayAccess (C.VarExpr $ C.Var (C.UnitType c) (nodeNameUniversal idx)) (C.VarExpr addrCounter)) 
            Realm.Global ->
              C.VarExpr $ C.Var (C.UnitType c) (nodeNameUniversal idx)
          else flip C.VarDefSub expr 
               (C.Var (C.UnitType c) $ nodeNameCursored env idx cursor)

    -- lhsCursors :: (Opt.Ready v g) => V.Vector(Set.Set(v g))
    lhsCursors = V.generate idxSize f
      where 
        f idx
          | not (Set.member idx allIdxSet) = Set.empty
          | Set.member idx outputIdxSet    = Set.singleton Additive.zero
          | otherwise                      = lhsRequest idx

    -- lhsRequests :: FGL.Node -> (Set.Set (v g))
    lhsRequest idx =
      Set.fromList $
      map snd $
      filter ((==idx) . fst) $
      concat $ 
      [snd $ rhsAndRequest env jdx cur| 
       jdx <- Set.toList allIdxSet, 
       jdx > idx,
       cur <- Set.toList $ lhsCursors V.! jdx
       ]

    -- rhsAndRequest :: (Opt.Ready v g) => Env v g -> FGL.Node -> v g -> (C.Expr,[(Int, v g)])
    rhsAndRequest env' idx cursor = 
      let (idxInst,inst) = case preInst idx of
            found:_ -> found
            _       -> error $ "right hand side is not inst:" ++ show idx
          prepre = map fst $ sortBy (\x y -> compare (snd x) (snd y)) $ FGL.lpre graph idxInst
          isInput = Set.member idx inputIdxSet
          creatVar idx = C.VarExpr $ C.Var C.UnknownType (nodeNameUniversal idx)
        in case inst of
      _ | isInput     -> case realm of
        Realm.Local -> (C.ArrayAccess (creatVar idx) (codecCursor cursor), [])
        Realm.Global -> (creatVar idx, [])
      OM.Imm dyn      -> (C.Imm dyn, [])
      OM.Arith op     -> (rhsArith op (map (nodeToRhs env' cursor) prepre),  
                      map (,cursor) prepre)
      OM.Shift v      -> case prepre of
        [pre1] -> (nodeToRhs env' cursor' pre1, [(pre1,cursor')]) where cursor' = cursor - v
        _      -> error $ "shift has not 1 pre!" ++ show idxInst ++  show prepre
      OM.LoadIndex ax -> (codecLoadIndex !! axisIndex ax, [])
      OM.LoadSize  ax -> (codecLoadSize  !! axisIndex ax, [])
      OM.Reduce op    -> let fname = T.pack ("reduce_" ++ map toLower (show op)) in
        (C.FuncCallStd fname (map creatVar prepre), [])
      OM.Broadcast    -> let fname = "broadcast" in
        (C.FuncCallStd fname (map creatVar prepre), [])
      _               -> (C.CommentExpr ("TODO : " ++ showT inst) (C.toDyn (42::Int)), [])

    nodeToRhs env' cursor idx = C.VarExpr $ C.Var C.UnknownType $ nodeNameCursored env' idx cursor


    preVal  = filterVal  . FGL.pre graph
    preInst = filterInst . FGL.pre graph
    sucVal  = filterVal  . FGL.suc graph
    sucInst = filterInst . FGL.suc graph

    filterVal  = concat . map (\(i,(xs,ys))-> map(i,)xs) . shiwake
    filterInst = concat . map (\(i,(xs,ys))-> map(i,)ys) . shiwake
    shiwake indices = 
      map (\idx -> (idx,) $ case FGL.lab graph idx of
              Just (OM.NValue dval _) -> ([dval], [])
              Just (OM.NInst  inst _) -> ([], [inst])
              Nothing                 -> error $ "not in graph:" ++ show idx) $
      indices 

    idxSize = FGL.noNodes graph

    allIdxSet = Set.unions [inputIdxSet, outputIdxSet, calcIdxSet]
    inputIdxSet  = Set.fromList $ V.toList $ Plan.inputIdxs  subker
    outputIdxSet = Set.fromList $ V.toList $ Plan.outputIdxs subker
    calcIdxSet = Set.fromList $ V.toList $ Plan.calcIdxs subker    

    graph = Plan.dataflow subker

-- | convert a DynValue to C type representation
mkCtyp :: Opt.Ready v g => Env v g -> DVal.DynValue -> C.TypeRep
mkCtyp env x = case x of
  DVal.DynValue Realm.Global c -> C.UnitType c
  DVal.DynValue Realm.Local  c -> containerType env c          

containerType :: Env v g -> TypeRep -> C.TypeRep
containerType (Env setup _) c = case Native.language setup of
  Native.CPlusPlus -> C.TemplateType "std::vector" [C.UnitType c]
  Native.CUDA      -> C.TemplateType "thrust::device_vector" [C.UnitType c]


-- | a universal naming rule for a node.
nodeNameUniversal :: FGL.Node -> Name
nodeNameUniversal idx = mkName $ "a" ++ showT idx



nodeNameCursored :: Opt.Ready v g => Env v g ->  FGL.Node -> v g -> Name
nodeNameCursored env idx cursor = mkName $ "a" ++ showT idx ++ "_" ++ 
                                cursorToText env cursor

cursorToText :: Opt.Ready v g => Env v g ->  v g -> T.Text
cursorToText _ cursor = cursorT
  where
    cursorT :: T.Text
    cursorT = foldl1 connector $ compose (\i -> T.map sanitize $ showT (cursor ! i))
    connector a b = a ++ "_" ++ b
    sanitize c
      | isDigit c = c
      | c == '-'  = 'm'
      | c == '.'  = 'd'
      | otherwise = 'k'



-- | Utility Types
intImm :: Int -> C.Expr
intImm = C.toDyn

tInt :: C.TypeRep
tInt = C.typeOf (undefined :: Int)

tSizet :: C.TypeRep
tSizet = C.typeOf (undefined :: Int)


tVoid :: C.TypeRep
tVoid = C.typeOf ()

tHostVecInt :: C.TypeRep
tHostVecInt = C.TemplateType "thrust::host_vector" [tInt]

tDeviceVecInt :: C.TypeRep
tDeviceVecInt = C.TemplateType "thrust::device_vector" [tInt]

rhsArith :: Arith.Operator -> [C.Expr] -> C.Expr
rhsArith op argExpr = case (op, argExpr) of
  (Arith.Identity, [x]) ->  x
  (Arith.Add    , [x,y]) -> C.Op2Infix "+" x y
  (Arith.Sub    , [x,y]) -> C.Op2Infix "-" x y
  (Arith.Neg    , [x]) -> C.Op1Prefix "-" x 
  (Arith.Mul    , [x,y]) -> C.Op2Infix "*" x y
  (Arith.Div    , [x,y]) -> C.Op2Infix "/" x y
  (Arith.Mod    , [x,y]) -> C.Op2Infix "%" x y  
  (Arith.Inv    , [x]) -> C.Op1Prefix "1/" x 
  (Arith.Not    , [x]) -> C.Op1Prefix "!" x   
  (Arith.And    , [x,y]) -> C.Op2Infix "&&" x y  
  (Arith.Or     , [x,y]) -> C.Op2Infix "||" x y  
  (Arith.EQ     , [x,y]) -> C.Op2Infix "==" x y  
  (Arith.NE     , [x,y]) -> C.Op2Infix "!=" x y    
  (Arith.LT     , [x,y]) -> C.Op2Infix "<" x y    
  (Arith.LE     , [x,y]) -> C.Op2Infix "<=" x y    
  (Arith.GT     , [x,y]) -> C.Op2Infix ">" x y    
  (Arith.GE     , [x,y]) -> C.Op2Infix ">=" x y    
  (Arith.Select , [x,y,z]) -> C.Op3Infix "?" ":" x y z   
  _ -> C.FuncCallStd (showT op) argExpr


library :: [C.Statement]
library = (:[]) $ C.Exclusive C.SourceFile $ C.RawStatement  "template <class T> T broadcast (const T& x) {\n  return x;\n}\ntemplate <class T> T reduce_sum (const std::vector<T> &xs) {\n  T ret = 0;\n  for (int i = 0; i < xs.size(); ++i) ret+=xs[i];\n  return ret;\n}\ntemplate <class T> T reduce_min (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::min(ret,xs[i]);\n  return ret;\n}\ntemplate <class T> T reduce_max (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::max(ret,xs[i]);\n  return ret;\n}\n"