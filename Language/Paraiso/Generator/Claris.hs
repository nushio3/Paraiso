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
import qualified Data.Text.IO as T
import           Language.Paraiso.Generator 
import           Language.Paraiso.Generator.ClarisDef 
import           Language.Paraiso.Generator.ClarisTrans 
import           Language.Paraiso.Name (nameStr)
import           Language.Paraiso.Prelude
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))

instance Generator Program where
  generate prog0 path = do
    let 
      headerFn :: FilePath
      headerFn     = path </> headerFnBody
      cppFn        = path </> cppFnBody
      headerFnBody = nameStr prog ++ ".hpp"
      cppFnBody    = nameStr prog ++ ".cpp"
      
      prog = prog0{topLevel = tlm}
      tlm0  = topLevel prog0
      
      pragmaOnce = Exclusive HeaderFile $ StmtPrpr $ PrprPragma "once"
      myHeader   = Exclusive SourceFile $ StmtPrpr $ PrprInclude Quotation2 $ T.pack headerFnBody
      
      tlm = addIfMissing pragmaOnce $ addIfMissing myHeader $ tlm0
      
      addIfMissing x xs = if x `elem` xs then xs else x:xs
      
    createDirectoryIfMissing True path
    T.writeFile headerFn $ translate headerFile prog
    T.writeFile cppFn    $ translate sourceFile prog
    return [headerFn, cppFn]
