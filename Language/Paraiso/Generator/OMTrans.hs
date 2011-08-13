{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.OMTrans (
  translate
  ) where

import           Language.Paraiso.Name
import qualified Language.Paraiso.OM as OM
import           Language.Paraiso.Generator.Plan

translate :: OM.OM v g a -> Plan v g a
translate om = Plan {
  planName = name om,
  setup    = OM.setup om,
  kernels  = OM.kernels om
  }

