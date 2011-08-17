{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate
  ) where

import qualified Data.Graph.Inductive                as FGL
import qualified Data.ListLike.String                as LL
import           Data.ListLike.Text ()
import qualified Data.Vector                         as V
import qualified Language.Paraiso.Generator.Claris   as C
import qualified Language.Paraiso.Generator.Native   as Native
import qualified Language.Paraiso.Generator.Plan     as Plan
import qualified Language.Paraiso.OM.DynValue        as DVal
import qualified Language.Paraiso.OM.Graph           as OM
import qualified Language.Paraiso.OM.Realm           as Realm
import qualified Language.Paraiso.Optimization.Graph as Opt
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

translate :: Opt.Ready v g => Native.Setup v g -> Plan.Plan v g a -> C.Program
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
    comments = (:[]) $ C.Comment $ LL.unlines [ 
      "",
      "negaMargin = " ++ showT (Plan.negaMargin plan),
      "posiMargin = " ++ showT (Plan.posiMargin plan)
      ]
    
    memberFuncs = V.toList $ V.map makeFunc $ Plan.kernels plan
    makeFunc ker = C.MemberFunc C.Public $ 
                   C.function tVoid (name ker)
    tVoid = C.typeOf ()
    
    subKernelFuncs = V.toList $ V.map makeSubFunc $ Plan.subKernels plan
    
    makeSubFunc subker = 
      C.MemberFunc C.Public $ 
      (C.function tVoid (name subker))
      { C.funcArgs = 
         makeSubArg True (Plan.labNodesIn subker) ++
         makeSubArg False (Plan.labNodesOut subker)
      }
    
    nodeNameUniversal :: FGL.Node -> Name
    nodeNameUniversal x = mkName $ "a_" ++ showT x
    
    makeSubArg isConst lnodes =
      let f = (if isConst then C.Const else id) . C.RefOf
      in
      map (\(idx,nd)-> case nd of
                OM.NValue typ _ -> C.Var (f $ mkCtyp typ) (nodeNameUniversal idx)
                _ -> error "NValue expected" ) $
      V.toList lnodes
    
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
        (mkCtyp $ Plan.storageType stRef) 
        (name stRef) 

    mkCtyp :: DVal.DynValue -> C.TypeRep
    mkCtyp x = case x of
      DVal.DynValue Realm.Global c -> C.UnitType c
      DVal.DynValue Realm.Local  c -> containerType c          
      
    containerType c = case Native.language setup of
      Native.CPlusPlus -> C.TemplateType "std::vector" [C.UnitType c]
      Native.CUDA      -> C.TemplateType "thrust::device_vector" [C.UnitType c]
    
          
