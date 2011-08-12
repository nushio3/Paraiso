{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
-- | [CLARIS] C++-Like Abstract Representation of Intermediate Syntax.
--
--  Claris connects the Orthotope Machine program to native languages
--  with capability to describe classes and containers, such as
--  C++. Claris may include


module Language.Paraiso.Generator.Claris (      
  module Language.Paraiso.Generator.ClarisDef,
  module Language.Paraiso.Generator.ClarisTrans
  ) where

import qualified Data.Text    as T
import           Language.Paraiso.Generator 
import           Language.Paraiso.Generator.ClarisDef 
import           Language.Paraiso.Generator.ClarisTrans 
import qualified Language.Paraiso.Generator.Native as Native
import           Language.Paraiso.Name (nameStr)
import           Language.Paraiso.Prelude

instance Generator Program where
  generate _ prog0 = 
    [ (headerFn, translate headerFile prog),
      (cppFn   , translate sourceFile prog)
    ]
    where
      headerFn :: FilePath
      headerFn  = nameStr prog ++ ".hpp"
      cppFn     = nameStr prog ++ "." ++ sourceExt
      sourceExt = case language prog0 of
        Native.CPlusPlus -> "cpp"
        Native.CUDA      -> "cu"
      
      prog = prog0{topLevel = tlm}
      tlm0  = topLevel prog0
      
      pragmaOnce = Exclusive HeaderFile $ StmtPrpr $ PrprPragma "once"
      myHeader   = Exclusive SourceFile $ StmtPrpr $ PrprInclude Quotation2 $ T.pack headerFn
      
      tlm = addIfMissing pragmaOnce $ addIfMissing myHeader $ tlm0
      
      addIfMissing x xs = if x `elem` xs then xs else x:xs
