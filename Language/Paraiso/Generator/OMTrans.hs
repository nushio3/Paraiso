{-# LANGUAGE  NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.OMTrans (
  translate
  ) where

import qualified Language.Paraiso.Annotation as Anot
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM as OM
import           Language.Paraiso.Generator.Plan
import qualified Language.Paraiso.Generator.Native as Native
import qualified Language.Paraiso.Optimization as Opt


translate :: (Opt.Ready v g) =>
  Native.Setup -> OM.OM v g Anot.Annotation -> Plan v g Anot.Annotation
translate setup om0 = Plan 
  { planName = name om,
    setup    = OM.setup om,
    kernels  = OM.kernels om
  }
  where
    om = Opt.optimize (Native.optLevel setup) om0
    