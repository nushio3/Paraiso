{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate
  ) where

import qualified Data.Vector                         as V
import qualified Language.Paraiso.Generator.Claris   as C
import qualified Language.Paraiso.Generator.Native   as Native
import qualified Language.Paraiso.Generator.Plan     as Plan
import qualified Language.Paraiso.OM.DynValue        as DVal
import qualified Language.Paraiso.OM.Realm           as Realm
import qualified Language.Paraiso.Optimization.Graph as Opt
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

translate :: Opt.Ready v g => Native.Setup -> Plan.Plan v g a -> C.Program
translate setup plan = 
  C.Program 
  { C.progName = name plan,
    C.topLevel = 
      map include stlHeaders ++ 
      [ C.ClassDef $ C.Class (name plan) $
        storageVars ++ subKernelFuncs ++ memberFuncs 
      ]
  }
  where
    memberFuncs = V.toList $ V.map makeFunc $ Plan.kernels plan
    makeFunc ker = C.MemberFunc C.Public $ 
                   C.function tVoid (name ker)
    tVoid = C.typeOf ()
    
    subKernelFuncs = V.toList $ V.map makeFunc $ Plan.subKernels plan
    
    include = C.Exclusive C.HeaderFile . C.StmtPrpr . C.PrprInclude C.Chevron
    stlHeaders = case Native.language setup of
      Native.CPlusPlus -> ["vector"]
      Native.CUDA      -> ["thrust/device_vector.h", "thrust/host_vector.h"]
    
    containerType c = case Native.language setup of
      Native.CPlusPlus -> C.TemplateType "std::vector" [C.UnitType c]
      Native.CUDA      -> C.TemplateType "thrust::device_vector" [C.UnitType c]
    
    
    storageVars = 
      V.toList $
      V.map storageRefToMenber $
      Plan.storages plan
    storageRefToMenber stRef =  
      C.MemberVar  C.Private (C.Var typ $ name stRef) 
      where
        typ = case Plan.storageType stRef of
          DVal.DynValue Realm.Global c -> C.UnitType c
          DVal.DynValue Realm.Local  c -> containerType c          
          
