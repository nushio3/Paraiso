{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude, OverloadedStrings, TupleSections #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate
  ) where


import qualified Algebra.Additive                    as Additive
import qualified Algebra.Ring                        as Ring
import           Data.Char
import           Data.Dynamic
import qualified Data.Graph.Inductive                as FGL
import qualified Data.ListLike.String                as LL
import           Data.ListLike.Text ()
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

    memberFuncs = V.toList $ V.map makeFunc $ Plan.kernels plan
    makeFunc ker = C.MemberFunc C.Public $ 
                   C.function tVoid (name ker)

    subKernelFuncs = V.toList $ V.map (makeSubFunc env) $ Plan.subKernels plan


    include = C.Exclusive C.HeaderFile . C.StmtPrpr . C.PrprInclude C.Chevron
    stlHeaders = case Native.language setup of
      Native.CPlusPlus -> ["vector"]
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


-- | Create a subKernel: a function that performs a portion of actual calculations.
makeSubFunc :: Opt.Ready v g => Env v g -> Plan.SubKernelRef v g AnAn -> C.MemberDef
makeSubFunc env subker = 
  C.MemberFunc C.Public $ 
  (C.function tVoid (name subker))
  { C.funcArgs = 
     makeSubArg env True (Plan.labNodesIn subker) ++
     makeSubArg env False (Plan.labNodesOut subker),
    C.funcBody = if Realm.realm subker == Realm.Global then [] else
      [ C.Comment $ LL.unlines 
        [ "",
          "lowerMargin = " ++ showT (Plan.lowerBoundary subker),
          "upperMargin = " ++ showT (Plan.upperBoundary subker)
        ],
        loopMaker env subker
      ]
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
loopMaker :: Opt.Ready v g => Env v g -> Plan.SubKernelRef v g AnAn -> C.Statement
loopMaker env@(Env setup plan) subker = 
  C.StmtFor 
    (C.VarDefSub ctr (intImm 0)) 
    (C.Op2Infix "<" (C.VarExpr ctr) (C.toDyn (vProduct rect)))
    (C.Op1Prefix "++" (C.VarExpr ctr)) 
    loopContent
  where
    ctr = C.Var tSizet (mkName "i")
    rect = Native.localSize setup + Plan.lowerMargin plan + Plan.upperMargin plan
     - Plan.lowerBoundary subker - Plan.upperBoundary subker


    loopContent = 
      concat $
      map buildExprs $
      filterVal $
      Set.toList allIdxSet

    buildExprs (idx, val@(DVal.DynValue r c)) = 
      map (\cursor -> 
            C.VarDefSub (C.Var (C.UnitType c) $nodeNameCursored env idx cursor)
            (fst $ rhsAndRequest env idx cursor)
          ) $
      Set.toList $ lhsCursors V.! idx
    
    
    lhsCursors :: (Opt.Ready v g) => V.Vector(Set.Set(v g))
    lhsCursors = V.generate idxSize f
      where 
        f idx
          | not (Set.member idx allIdxSet) = Set.empty
          | Set.member idx outputIdxSet    = Set.singleton Additive.zero
          | otherwise                      = Set.unions $ map (lhsCursors V.!) $ FGL.suc graph idx
    
    rhsAndRequest :: (Opt.Ready v g) 
                     => Env v g
                     -> FGL.Node
                     -> v g
                     -> (C.Expr,[(Int, Set.Set(v g))])
    rhsAndRequest env' idx cursor = 
      let (idxPre,inst) = case preInst idx of
            found:_ -> found
            _       -> error $ "right hand side is not inst:" ++ show idx
        in case inst of
      OM.Imm dyn  -> (C.Imm dyn, [])
      OM.Arith op -> (rhsArith op (map (nodeToRhs env' cursor) (FGL.pre graph idxPre)),  
                      [])
      _           -> (C.CommentExpr ("TODO : " ++ showT inst) (C.toDyn (42::Int)), [])
        
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


vProduct :: (Vector v, Ring.C a) => (v a) -> a
vProduct = foldl (*) Ring.one 



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

