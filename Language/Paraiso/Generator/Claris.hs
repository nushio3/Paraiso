{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the Orthotope Machine program to native languages
--  with capability to describe classes and containers, such as
--  C++. Claris may include


module Language.Paraiso.Generator.Claris
    (
      Program, translate
    ) where

import           Language.Paraiso.Name
import           Language.Paraiso.OM (OM)
import qualified Language.Paraiso.OM as OM
import           Language.Paraiso.OM.Graph (Setup) 
import           Language.Paraiso.Prelude 

data Program vector gauge anot = 
  Program {
    programName :: Name,
    setup :: Setup  vector gauge anot 
    } deriving (Eq, Show)

instance Nameable (Program v g a) where
  name = programName


translate :: OM vector gauge anot -> Program vector gauge anot 
translate om = Program {programName = name om, setup = OM.setup om}
