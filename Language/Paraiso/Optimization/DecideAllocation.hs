{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Optimization.DecideAllocation (
  decideAllocation
  ) where

import Language.Paraiso.Annotation.Allocation
import Language.Paraiso.Prelude
import Language.Paraiso.OM
import Language.Paraiso.Optimization.Graph

decideAllocation :: Optimization
decideAllocation = id


