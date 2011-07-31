{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the Orthotope Machine program to native languages
--  with capability to describe classes and containers, such as
--  C++. Claris may include


module Language.Paraiso.Generator.Claris
    (
      Claris, translate
    ) where

import           Language.Paraiso.Name
import           Language.Paraiso.OM (OM)
import qualified Language.Paraiso.OM as OM
import           Language.Paraiso.OM.Graph (Setup) 

data Claris vector gauge anot = 
  Claris {
    clarisName :: Name,
    setup :: Setup  vector gauge anot 
    }

instance Nameable (Claris v g a) where
  name = clarisName


translate :: OM vector gauge anot -> Claris vector gauge anot 
translate om = Claris {clarisName = name om, setup = OM.setup om}