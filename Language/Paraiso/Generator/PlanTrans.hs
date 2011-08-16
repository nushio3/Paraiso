{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.PlanTrans (
  translate
  ) where

import qualified Data.Vector                       as V
import qualified Language.Paraiso.Generator.Claris as C
import qualified Language.Paraiso.Generator.Native as Native
import qualified Language.Paraiso.Generator.Plan   as Plan
import           Language.Paraiso.Name
import           Language.Paraiso.Prelude

translate :: Native.Setup -> Plan.Plan v g a -> C.Program
translate setup plan = 
  C.Program 
  { C.progName = name plan,
    C.topLevel = 
    [ C.ClassDef $ C.Class (name plan) $
      subKernelFuncs ++ memberFuncs 
    ]
  }
  where
    memberFuncs = V.toList $ V.map makeFunc $ Plan.kernels plan
    makeFunc ker = C.MemberFunc C.Public $ 
                   C.function tVoid (name ker)
    tVoid = C.typeOf ()
    
    subKernelFuncs = V.toList $ V.map makeFunc $ Plan.subKernels plan
    
    
    
    