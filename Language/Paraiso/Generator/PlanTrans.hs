{-# LANGUAGE CPP, DeriveDataTypeable, ExistentialQuantification, 
NoImplicitPrelude, OverloadedStrings, 
TupleSections #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate,
  commonLibraries
  ) where


import qualified Algebra.Additive                    as Additive
import           Data.Char
import           Data.Dynamic
import qualified Data.Graph.Inductive                as FGL
import           Data.List (sortBy)
import qualified Data.ListLike.String                as LL
import           Data.ListLike.Text ()
import qualified Data.Foldable                       as F
import           Data.Maybe
import qualified Data.Set                            as Set
import           Data.Tensor.TypeLevel
import qualified Data.Tensor.TypeLevel.Axis          as Axis
import qualified Data.Text                           as T
import qualified Data.Traversable                    as F
import qualified Data.Vector                         as V
import qualified Language.Paraiso.Annotation         as Anot
import qualified Language.Paraiso.Annotation.SyncThreads as Sync
import qualified Language.Paraiso.Generator.Claris   as C
import qualified Language.Paraiso.Generator.Native   as Native
import qualified Language.Paraiso.Generator.Plan     as Plan
import qualified Language.Paraiso.OM                 as OM
import qualified Language.Paraiso.OM.Arithmetic      as Arith
import qualified Language.Paraiso.OM.DynValue        as DVal
import qualified Language.Paraiso.OM.Graph           as OM
import qualified Language.Paraiso.OM.Realm           as Realm
import qualified Language.Paraiso.Optimization.Graph as Opt
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude hiding (Boolean(..))
import           NumericPrelude hiding ((++))

-- the standard annotation type.
type AnAn = Anot.Annotation

-- the set of variable needed to construct various things,
-- this makes the functions a bit tedious...
data Env v g = Env (Native.Setup v g) (Plan.Plan v g AnAn)

-- translate the plan to Claris
translate :: Opt.Ready v g => Native.Setup v g -> Plan.Plan v g AnAn -> C.Program
translate setup plan = 
  C.Program 
  { C.progName = name plan,
    C.topLevel = 
      stlHeaders ++ 
      library env ++ 
      comments ++
      subHelperFuncs ++ 
      [ C.ClassDef $ C.Class (name plan) $
        map fst storageVars ++ 
        constructorDef ++ 
        accessorsForSize env ++
        accessorsForVars env ++
        subMemberFuncs ++ memberFuncs 
      ]
  }
  where
    env = Env setup plan
    comments = (:[]) $ C.Comment $ LL.unlines [ 
      "",
      "lowerMargin = " ++ showT (Plan.lowerMargin plan),
      "upperMargin = " ++ showT (Plan.upperMargin plan)
      ]

    constructorDef = 
      (:[]) $
      C.MemberFunc C.Public True $
      (C.function  C.ConstructorType (name plan))
      { C.funcMemberInitializer = -- allocate memory for storage members
           concat $
           flip map storageVars $ \(memb, stRef) -> 
             case (memb, Realm.realm $ Plan.storageDynValue stRef) of
               (C.MemberVar _ var, Realm.Array)
                 -> [C.FuncCallUsr (name var) [C.FuncCallUsr (name (omFuncMemorySizeTotal env)) []]]
               _ -> []
      }

    memberFuncs = 
      V.toList $ 
      V.imap (\idx ker -> makeKernelFunc env idx ker) $ 
      Plan.kernels plan

    subHelperFuncs = concat $ V.toList $ V.map snd $ subKernelFuncs   
    subMemberFuncs = concat $ V.toList $ V.map fst $ subKernelFuncs   
    subKernelFuncs = V.map (makeSubFunc env) $ Plan.subKernels plan


    include   = C.Exclusive C.HeaderFile . C.StmtPrpr . C.PrprInclude C.Chevron
    include'' = C.Exclusive C.HeaderFile . C.StmtPrpr . C.PrprInclude C.Quotation2
    stlHeaders = case Native.language setup of
      Native.CPlusPlus -> map include ["algorithm", "cmath", "vector"]
      Native.CUDA      -> map include ["thrust/device_vector.h", "thrust/host_vector.h", "thrust/functional.h", "thrust/extrema.h", "thrust/reduce.h"] ++ 
                          map (include'' . T.pack . fst) commonLibraries

    storageVars = 
      V.toList $
      V.map storageRefToMember $
      Plan.storages plan
    storageRefToMember stRef =  
      (,stRef) $
      C.MemberVar C.Private $ 
      C.Var 
        (mkCppType env $ Plan.storageType stRef) 
        (name stRef) 

-- | Generate member functions for accessing the static variables
accessorsForVars :: Opt.Ready v g => Env v g -> [C.MemberDef]
accessorsForVars env@(Env setup plan) =
  map (C.MemberFunc C.Public True) $ do
    -- list monad
    stRef <- (V.toList $ Plan.storages plan)
    
    -- we have accessors only for static values.
    Just na <- [accessorName stRef]
    
    -- for all static values we have simple accessors that returns reference to it.
    let typeSimple = C.RefOf $ mkCppType env $ Plan.storageType stRef
    let bodySimple = [C.StmtReturn $ mkVarExpr $ nameText stRef]
    let argsSimple = []
        
    -- we also have elemental accessors, but only for Arrays.    
    let typeElemMaybe = case Plan.storageDynValue stRef of 
          DVal.DynValue Realm.Scalar _ -> []          
          DVal.DynValue Realm.Array  c -> [C.RefOf $ C.UnitType c]

    let argsElem = compose (\ax@(Axis i) -> 
                             C.Var (C.typeOf (Native.localSize setup ! ax))
                                   (mkName $ "i" ++ showT i))
        bodyElem = (:[]) $
                   C.StmtReturn $ C.ArrayAccess (mkVarExpr $ nameText stRef) $
                   productedArgs ! Axis 0
        productedArgs = compose 
          (\i -> 
            if (Axis.next i == Axis 0) 
            then (marginedArgs!i) 
            else C.Op2Infix "+" (marginedArgs!i) $
                 C.Op2Infix "*" (C.FuncCallUsr (name $ omFuncMemorySize env ! i) [])
                                (productedArgs!(Axis.next i)))
        marginedArgs = compose (\i -> C.Op2Infix "+" 
                                  (C.FuncCallUsr (name $ omFuncLowerMargin env ! i) [])  
                                  (C.VarExpr (argsElem ! i)))
                   
    let materials = (typeSimple, bodySimple, argsSimple) : fmap (,bodyElem,F.toList argsElem) typeElemMaybe
        
    (typ, bod, arg) <- materials
    
    return (C.function typ na){C.funcBody = bod, C.funcArgs = arg}    

accessorName :: (Plan.StorageRef v g a) -> Maybe Name    
accessorName x = case Plan.storageIdx x of
    Plan.StaticRef i     -> Just $ name $ OM.staticValues (Plan.setup $ Plan.parent x) V.! i
    Plan.ManifestRef i j -> Nothing



-- | Generate member functions that returns the sizes of the mesh
accessorsForSize :: Opt.Ready v g => Env v g -> [C.MemberDef]
accessorsForSize env =
  map (C.MemberFunc C.Public True) $
  ([omFuncLocalSizeTotal env, omFuncMemorySizeTotal env] ++) $
  concat $ 
  map F.toList $
  [omFuncLocalSize env, omFuncMemorySize env, omFuncLowerMargin env, omFuncUpperMargin env]

omFuncLocalSizeTotal :: Opt.Ready v g => Env v g -> C.Function                               
omFuncLocalSize      :: Opt.Ready v g => Env v g -> v C.Function
omFuncLocalSizeTotal = fst $ makeOmSizeFuncSet "om_size" (\(Env setup _) -> Native.localSize setup)
omFuncLocalSize      = snd $ makeOmSizeFuncSet "om_size" (\(Env setup _) -> Native.localSize setup)

omFuncMemorySizeTotal :: Opt.Ready v g => Env v g -> C.Function                               
omFuncMemorySize      :: Opt.Ready v g => Env v g -> v C.Function
omFuncMemorySizeTotal = fst $ makeOmSizeFuncSet "om_memory_size" 
                        (\(Env setup plan) -> Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan)
omFuncMemorySize      = snd $ makeOmSizeFuncSet "om_memory_size"
                        (\(Env setup plan) -> Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan)

omFuncLowerMargin     :: Opt.Ready v g => Env v g -> v C.Function
omFuncLowerMargin     = snd $ makeOmSizeFuncSet "om_lower_margin" (\(Env _ plan) -> Plan.lowerMargin plan)
omFuncUpperMargin     :: Opt.Ready v g => Env v g -> v C.Function
omFuncUpperMargin     = snd $ makeOmSizeFuncSet "om_upper_margin" (\(Env _ plan) -> Plan.upperMargin plan)



makeOmSizeFuncSet :: Opt.Ready v g => 
                        Text              -- ^ The header text
                     -> (Env v g -> v g)  -- ^ How to read the size from the environment
                     -> (Env v g -> C.Function, Env v g -> v C.Function)
makeOmSizeFuncSet header sizeVecReader = (prodFunc, elemFuncs)
  where
    -- reader monad
    prodFunc = do
      sizeVec <- sizeVecReader
      trivialFunc header $ product $ F.toList sizeVec
    elemFuncs = do
      sizeVec <- sizeVecReader
      F.sequenceA $ compose (\i -> trivialFunc (header ++ "_" ++ showT (axisIndex i)) (sizeVec ! i)) 

    trivialFunc str ret = do
      sizeVec <- sizeVecReader
      gt <- gaugeType
      return $ (C.function gt $ mkName $ str)
          { C.funcBody = 
            [ C.StmtReturn (C.toDyn ret)
            ]
          }

-- | Generate member functions for accessing 
      

gaugeType :: Opt.Ready v g => Env v g -> C.TypeRep
gaugeType env@(Env setup plan) = C.typeOf $ Native.localSize setup ! (Axis 0)


-- Make Kernel Functions
makeKernelFunc :: Opt.Ready v g => Env v g -> Int -> OM.Kernel v g AnAn -> C.MemberDef
makeKernelFunc env@(Env setup plan) kerIdx ker = C.MemberFunc C.Public False $ 
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
         (C.VarExpr $ C.Var (mkCppType env $ Plan.storageType stRef) (name stRef) )
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
        (mkCppType env $ Plan.storageType stRef) 
        (name stRef) 


-- | Create a subKernel: a member function that performs a portion of
--   actual calculations. It may also generate some helper functions
--   called from the subKernel body.
makeSubFunc :: Opt.Ready v g 
            => Env v g 
            -> Plan.SubKernelRef v g AnAn 
            -> ([C.MemberDef], [C.Statement])
makeSubFunc env@(Env setup plan) subker = 
  case Native.language setup of
    Native.CPlusPlus -> cppSolution
    Native.CUDA      -> cudaSolution
  where
    rlm = Realm.realm subker 
    
    cudaHelperName = mkName $ nameText subker ++ "_inner"
    
    cudaBodys = 
      if rlm == Realm.Scalar 
      then []
      else
        (:[]) $
        C.FuncDef $
        (C.function 
         (C.QualifiedType [C.CudaGlobal] tVoid) 
         cudaHelperName)
        { C.funcArgs = 
           makeRawSubArg env True  (Plan.labNodesIn subker) ++
           makeRawSubArg env False (Plan.labNodesOut subker),
          C.funcBody = 
          [ C.Comment $ LL.unlines 
            [ "",
              "lowerMargin = " ++ showT (Plan.lowerBoundary subker),
              "upperMargin = " ++ showT (Plan.upperBoundary subker)
            ]
          ] ++ loopMaker env rlm subker
        }
    
    (gridDim, blockDim) = Native.cudaGridSize setup
    
    cudaSolution = 
      (,cudaBodys) $
      (:[]) $
      C.MemberFunc C.Public False $ 
      (C.function tVoid (name subker))
      { C.funcArgs = 
         makeSubArg env True (Plan.labNodesIn subker) ++
         makeSubArg env False (Plan.labNodesOut subker),
        C.funcBody = 
          if rlm == Realm.Scalar 
          then loopMaker env rlm subker
          else
            [ C.Comment $ LL.unlines 
              [ "",
                "lowerMargin = " ++ showT (Plan.lowerBoundary subker),
                "upperMargin = " ++ showT (Plan.upperBoundary subker)
              ],
              C.RawStatement "{static bool fst = false;",
              C.StmtExpr $ C.FuncCallStd "if (fst) cudaFuncSetCacheConfig" 
                 [mkVarExpr $ nameText cudaHelperName, mkVarExpr "cudaFuncCachePreferL1"],
              C.RawStatement "fst = true;}",
              C.StmtExpr $ C.CudaFuncCallUsr cudaHelperName (C.toDyn gridDim) (C.toDyn blockDim) $
              map takeRaw $ 
              (V.toList $ Plan.labNodesIn subker) ++ (V.toList $ Plan.labNodesOut subker)
            ] 
      }

    takeRaw (idx, nd)= 
      case nd of
        OM.NValue typ _ -> 
          extractor typ $
          C.VarExpr $
          C.Var (mkCppType env typ) (nodeNameUniversal idx)
        _ -> error "NValue expected" 

    extractor typ = case Realm.realm typ of
      Realm.Array ->           
        flip C.MemberAccess (C.FuncCallStd "raw" []) 
      Realm.Scalar ->
        id

    cppSolution = 
      (,[]) $
      (:[]) $
      C.MemberFunc C.Public False $ 
      (C.function tVoid (name subker))
      { C.funcArgs = 
         makeSubArg env True (Plan.labNodesIn subker) ++
         makeSubArg env False (Plan.labNodesOut subker),
        C.funcBody = 
          if rlm == Realm.Scalar 
          then loopMaker env rlm subker
          else
            [ C.Comment $ LL.unlines 
              [ "",
                "lowerMargin = " ++ showT (Plan.lowerBoundary subker),
                "upperMargin = " ++ showT (Plan.upperBoundary subker)
              ]
            ] ++ loopMaker env rlm subker
      }

    
    
-- | make a subroutine argument list.
makeSubArg :: Opt.Ready v g => Env v g -> Bool -> V.Vector (FGL.LNode (OM.Node v g AnAn)) -> [C.Var]
makeSubArg env isConst lnodes =
  let f = (if isConst then C.Const else id) . C.RefOf
  in
  map (\(idx,nd)-> case nd of
            OM.NValue typ _ -> C.Var (f $ mkCppType env typ) (nodeNameUniversal idx)
            _ -> error "NValue expected" ) $
  V.toList lnodes

-- | make a subroutine argument list, using raw pointers.
makeRawSubArg :: Opt.Ready v g => Env v g -> Bool -> V.Vector (FGL.LNode (OM.Node v g AnAn)) -> [C.Var]
makeRawSubArg env isConst lnodes =
  let f = (if isConst then C.Const else id) 
  in
  map (\(idx,nd)-> case nd of
          OM.NValue typ _ -> 
            C.Var (f $ mkCudaRawType env typ) (nodeNameUniversal idx)
          _ -> error "NValue expected" ) $
  V.toList lnodes




-- | implement the loop for each subroutine
loopMaker :: Opt.Ready v g => Env v g -> Realm.Realm -> Plan.SubKernelRef v g AnAn -> [C.Statement]
loopMaker env@(Env setup plan) realm subker = case realm of
  Realm.Array ->
    pragma ++
    [ C.StmtFor 
      (C.VarDefSub loopCounter loopBegin) 
      (C.Op2Infix "<"  (C.VarExpr loopCounter) loopEnd)
      (C.Op2Infix "+=" (C.VarExpr loopCounter) loopStride) $
      [C.VarDefSub addrCounter codecAddr] ++
      loopContent
    ]
  Realm.Scalar -> loopContent

  where
    pragma = 
      if Native.language setup == Native.CPlusPlus 
      then [C.StmtPrpr $ C.PrprPragma "omp parallel for"]
      else []
    (loopBegin, loopEnd, loopStride) = case Native.language setup of
      Native.CPlusPlus -> (intImm 0, C.toDyn (product boundarySize), intImm 1)
      Native.CUDA -> (loopBeginCuda, C.toDyn (product boundarySize), loopStrideCuda)      
    loopBeginCuda = mkVarExpr "blockIdx.x * blockDim.x + threadIdx.x"
    loopStrideCuda   = mkVarExpr "blockDim.x * gridDim.x"    
    
    loopCounter = C.Var tSizet (mkName "i")
    memorySize   = F.toList $ Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan
    boundarySize = F.toList $ Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan
     - Plan.lowerBoundary subker - Plan.upperBoundary subker

    codecDiv = 
      [ if idx == 0 then (C.VarExpr loopCounter) else C.Op2Infix "/" (C.VarExpr loopCounter) (C.toDyn $ product $ take idx boundarySize) 
      | idx <- [0..length boundarySize-1]]
    codecMod = 
      [ if idx == length codecDiv-1 then x else C.Op2Infix "%" x (C.toDyn $ boundarySize !! idx)
      | (idx, x) <- zip [0..] codecDiv]
    codecModAdd = 
      [ C.Op2Infix "+" x (C.toDyn $ Plan.lowerMargin plan ! (Axis idx))
      | (idx, x) <- zip [0..] codecMod]
    codecAddr = 
      if memorySize == boundarySize 
      then C.VarExpr loopCounter
      else foldl1 (C.Op2Infix "+")
           [ C.Op2Infix "*" x (C.toDyn $ product $ take idx  memorySize)
           | (idx, x) <- zip [0..] codecModAdd]
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
      addSyncFunctions idx  $
      map (\cursor -> 
            lhs cursor
            (fst $ rhsAndRequest env idx cursor)
          ) $
      Set.toList $ lhsCursors V.! idx
      where
        lhs cursor expr = 
          if Set.member idx outputIdxSet
          then C.StmtExpr $ flip (C.Op2Infix "=") expr $ case realm of
            Realm.Array ->
              (C.ArrayAccess (C.VarExpr $ C.Var (C.UnitType c) (nodeNameUniversal idx)) (C.VarExpr addrCounter)) 
            Realm.Scalar ->
              C.VarExpr $ C.Var (C.UnitType c) (nodeNameUniversal idx)
          else flip C.VarDefSub expr 
               (C.Var (C.UnitType c) $ nodeNameCursored env idx cursor)

    addSyncFunctions :: FGL.Node -> [C.Statement] -> [C.Statement] 
    addSyncFunctions idx = 
      if realm /= Realm.Array ||
         Native.language setup /= Native.CUDA 
         then id
         else foldl (.) id $ map adder anot
      where
        anot :: [Sync.Timing]
        anot =  (maybeToList $ FGL.lab graph idx) >>= (Anot.toList . OM.getA)
          
        adder :: Sync.Timing -> [C.Statement] -> [C.Statement]  
        adder Sync.Pre  = ([C.RawStatement "__syncthreads();"] ++ )
        adder Sync.Post = ( ++ [C.RawStatement "__syncthreads();"])

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
          creatVar idx' = C.VarExpr $ C.Var C.UnknownType (nodeNameUniversal idx')
        in case inst of
      _ | isInput     -> case realm of
        Realm.Array -> (C.ArrayAccess (creatVar idx) (codecCursor cursor), [])
        Realm.Scalar -> (creatVar idx, [])
      OM.Imm dyn      -> (C.Imm dyn, [])
      OM.Arith op     -> (rhsArith env' op (map (nodeToRhs env' cursor) prepre),  
                      map (,cursor) prepre)
      OM.Shift v      -> case prepre of
        [pre1] -> (nodeToRhs env' cursor' pre1, [(pre1,cursor')]) where cursor' = cursor - v
        _      -> error $ "shift has not 1 pre!" ++ show idxInst ++  show prepre
      OM.LoadIndex ax -> (codecLoadIndex !! axisIndex ax, [])
      OM.LoadSize  ax -> (codecLoadSize  !! axisIndex ax, [])
      OM.Reduce op    -> let fname = T.pack ("om_reduce_" ++ map toLower (show op)) in
        (C.FuncCallStd fname (map creatVar prepre), [])
      OM.Broadcast    -> let fname = "om_broadcast" in
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
mkCppType :: Opt.Ready v g => Env v g -> DVal.DynValue -> C.TypeRep
mkCppType env x = case x of
  DVal.DynValue Realm.Scalar c -> C.UnitType c
  DVal.DynValue Realm.Array  c -> containerType env c          

containerType :: Env v g -> TypeRep -> C.TypeRep
containerType (Env setup _) c = case Native.language setup of
  Native.CPlusPlus -> C.TemplateType "std::vector" [C.UnitType c]
  Native.CUDA      -> C.TemplateType "thrust::thrust_vector" [C.UnitType c]


-- | convert a DynValue to raw-pointer type representation 
--   for example used within CUDA kernel
mkCudaRawType :: Opt.Ready v g => Env v g -> DVal.DynValue -> C.TypeRep
mkCudaRawType env x = case x of
  DVal.DynValue Realm.Scalar c -> C.UnitType c
  DVal.DynValue Realm.Array  c -> containerRawType env c          

containerRawType :: Env v g -> TypeRep -> C.TypeRep
containerRawType (Env setup _) c = case Native.language setup of
  Native.CPlusPlus -> C.PtrOf $ C.UnitType c
  Native.CUDA      -> C.PtrOf $ C.UnitType c



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
    cursorT = F.foldl1 connector $ compose (\i -> T.map sanitize $ showT (cursor ! i))
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

mkVarExpr :: Text -> C.Expr
mkVarExpr = C.VarExpr . C.Var C.UnknownType . mkName

tVoid :: C.TypeRep
tVoid = C.typeOf ()

tHostVecInt :: C.TypeRep
tHostVecInt = C.TemplateType "thrust::host_vector" [tInt]

tDeviceVecInt :: C.TypeRep
tDeviceVecInt = C.TemplateType "thrust::device_vector" [tInt]

rhsArith :: Opt.Ready v g => Env v g -> Arith.Operator -> [C.Expr] -> C.Expr
rhsArith (Env setup _) op argExpr = case (op, argExpr) of
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
  (Arith.Max    , [x,y])  -> C.FuncCallStd (nmsp "std::max" "max") [x,y]
  (Arith.Min    , [x,y])  -> C.FuncCallStd (nmsp "std::min" "min") [x,y]
  (Arith.Abs    , [x])  -> C.FuncCallStd "abs" [x]
  (Arith.Sqrt   , [x])  -> C.FuncCallStd "sqrt" [x]
  (Arith.Exp    , [x])  -> C.FuncCallStd "exp" [x]
  (Arith.Log    , [x])  -> C.FuncCallStd "log" [x]
  (Arith.Sin    , [x])  -> C.FuncCallStd "sin" [x]
  (Arith.Cos    , [x])  -> C.FuncCallStd "cos" [x]
  (Arith.Tan    , [x])  -> C.FuncCallStd "tan" [x]
  (Arith.Asin   , [x])  -> C.FuncCallStd "asin" [x]
  (Arith.Acos   , [x])  -> C.FuncCallStd "acos" [x]
  (Arith.Atan   , [x])  -> C.FuncCallStd "atan" [x]
  (Arith.Atan2  , [x,y])  -> C.FuncCallStd "atan2" [x,y]
  _ -> C.FuncCallStd (T.map toLower $ showT op) argExpr
  where
    nmsp a b = case Native.language setup of
      Native.CPlusPlus -> a
      Native.CUDA      -> b

library :: Opt.Ready v g => Env v g -> [C.Statement]
library (Env setup _) = (:[]) $ C.Exclusive C.SourceFile $ C.RawStatement $ lib
  where
    lib = case Native.language setup of
      Native.CPlusPlus -> cpuLib
      Native.CUDA      -> gpuLib
    -- use draft.cpp to generate library
    cpuLib = "\ntemplate <class T> T om_broadcast (const T& x) {\n  return x;\n}\ntemplate <class T> T om_reduce_sum (const std::vector<T> &xs) {\n  T ret = 0;\n  for (int i = 0; i < xs.size(); ++i) ret+=xs[i];\n  return ret;\n}\ntemplate <class T> T om_reduce_min (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::min(ret,xs[i]);\n  return ret;\n}\ntemplate <class T> T om_reduce_max (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::max(ret,xs[i]);\n  return ret;\n}\n"
    gpuLib = "\ntemplate <class T> __device__ __host__ T om_broadcast (const T& x) {\n  return x;\n}\ntemplate <class T> T om_reduce_sum (const thrust::device_vector<T> &xs) {\n  return thurust::reduce(xs.begin(), xs.end(), 0, thrust::plus<T>());\n}\ntemplate <class T> T om_reduce_min (const thrust::device_vector<T> &xs) {\n  return *(thurust::min_element(xs.begin(), xs.end()));\n}\ntemplate <class T> T om_reduce_max (const thrust::device_vector<T> &xs) {\n  return *(thurust::max_element(xs.begin(), xs.end()));\n}\n\n"    

commonLibraries :: [(FilePath, Text)]
commonLibraries = [("om_thrust_vector.h", thrustVectorLib)]

thrustVectorLib :: Text
thrustVectorLib = "#pragma once\n\n#include <thrust/device_vector.h>\n#include <thrust/host_vector.h>\n#include <thrust/device_ptr.h>\n#include <iostream>\n\nnamespace thrust {\n\ntemplate <class T>\nclass thrust_vector {\npublic:\n  enum NewerFlag {\n    kHost,\n    kDevice,\n    kBoth\n  };\nprivate:\n  mutable thrust::host_vector<T> hv;\n  mutable thrust::device_vector<T> dv;\n  mutable NewerFlag newer_;\npublic:  \n\n  typedef typename thrust::host_vector<T>::size_type  size_type;\n  typedef T value_type;\n\n  thrust_vector () : newer_(kBoth) {}\n  thrust_vector (size_type n, const value_type &value = value_type()) :\n    hv(n, value), dv(n, value), newer_(kBoth) {}\n  thrust_vector (const thrust_vector<T> &v) :\n    hv(v.hv), dv(v.dv), newer_(kBoth) {}\n\n  const size_type size() const { return hv.size(); }\n\n  void bring_device () const {\n    if (kHost == newer_) {\n      dv = hv;\n      newer_ = kBoth;\n    }\n  }\n  void bring_host () const {\n    if (kDevice == newer_) {\n      hv = dv;\n      newer_ = kBoth;\n    }\n  }\n\n  thrust::host_vector<T> &host_vector() {\n    bring_host();\n    return hv;\n  }\n  thrust::device_vector<T> &device_vector() {\n    bring_device();\n    return dv;\n  }\n  \n  const T& operator[](const size_type n) const {\n    bring_host();\n    return hv[n];\n  }\n  T& operator[](const size_type n) {\n    bring_host();\n    newer_ = kHost;\n    return hv[n];\n  }\n  T* unsafe_raw_device () const {\n    return thrust::raw_pointer_cast(&*dv.begin());\n  }\n  T* unsafe_raw_host () const {\n    return &hv[0];\n  }\n  void unsafe_set_newer (NewerFlag n) {\n    newer_ = n;\n  }\n  \n  T* raw () const {\n    bring_device();\n    newer_ = kDevice;\n    return thrust::raw_pointer_cast(&*dv.begin());\n  }\n  typename thrust::device_vector<T>::iterator device_begin () {\n    bring_device();\n    return dv.begin();\n  }\n  typename thrust::device_vector<T>::iterator device_end () {\n    bring_device();\n    return dv.end();\n  }\n};\n\ntemplate<class T>\nT* raw(thrust::device_vector<T> &dv) {\n  return thrust::raw_pointer_cast(&*dv.begin());\n}\n\ntemplate<class T>\nT* raw(thrust::host_vector<T> &hv) {\n  return &hv[0];\n}\n\ntemplate<class T>\nT* raw(const thrust::thrust_vector<T> &tv) {\n  return tv.raw();\n}\n\n}\n"
    
    --    gpuLib =  "template <class T>\n__device__ __host__\nT broadcast (const T& x) {\n  return x;\n}\ntemplate <class T> T reduce_sum (const std::vector<T> &xs) {\n  T ret = 0;\n  for (int i = 0; i < xs.size(); ++i) ret+=xs[i];\n  return ret;\n}\ntemplate <class T> T reduce_min (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::min(ret,xs[i]);\n  return ret;\n}\ntemplate <class T> T reduce_max (const std::vector<T> &xs) {\n  T ret = xs[0];\n  for (int i = 1; i < xs.size(); ++i) ret=std::max(ret,xs[i]);\n  return ret;\n}\n" ++ "template <class T> T reduce_sum (const thrust::device_vector<T> &xs) {\n  return thrust::reduce(xs.begin(), xs.end(), 0, thrust::plus<T>());\n}\ntemplate <class T> T reduce_min (const thrust::device_vector<T> &xs) {\n  return *(thrust::min_element(xs.begin(), xs.end()));\n}\ntemplate <class T> T reduce_max (const thrust::device_vector<T> &xs) {\n  return *(thrust::max_element(xs.begin(), xs.end()));\n}\n\n"
