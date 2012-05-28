{-# LANGUAGE CPP,  FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
-- | a general code generator definition.
module Language.Paraiso.Generator
    (
      generate, generateIO
    ) where

import           Control.Monad
import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Generator.Claris      as C
import qualified Language.Paraiso.Generator.ClarisTrans as C
import qualified Language.Paraiso.Generator.Native      as Native
import qualified Language.Paraiso.Generator.OMTrans     as OM
import qualified Language.Paraiso.Generator.PlanTrans   as Plan
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM                    as OM
import qualified Language.Paraiso.Optimization          as Opt
import           Language.Paraiso.Prelude
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))
import           Prelude hiding ((++))


-- | Perform the code generation and returns the list of written
-- filepaths and their contents, for your interest.
generateIO :: (Opt.Ready v g) => Native.Setup v g -> OM.OM v g Anot.Annotation -> IO [(FilePath, Text)]
generateIO setup om = do
  let dir = Native.directory setup
  createDirectoryIfMissing True dir
  forM (generate setup om) $ \ (fn, con) -> do
    let absfn = dir </> fn
    T.writeFile absfn con
    return (absfn, con)

-- | Generate the (filename, content) list from a code generation
-- setup and a orthotope machine definition.
    
generate :: (Opt.Ready v g) => Native.Setup v g -> OM.OM v g Anot.Annotation -> [(FilePath, Text)]
generate setup om =  
  [ (headerFn, C.translate C.headerFile prog),
    (cppFn   , C.translate C.sourceFile prog)
  ] ++ Plan.commonLibraries
  where
    prog0 = 
      Plan.translate setup $
      OM.translate setup om

    tlm0  = C.topLevel prog0
    prog = prog0{C.topLevel = tlm}

    tlm = addIfMissing pragmaOnce $ addIfMissing myHeader $ tlm0

    pragmaOnce = C.Exclusive C.HeaderFile $ C.StmtPrpr $ C.PrprPragma "once"
    myHeader   = C.Exclusive C.SourceFile $ C.StmtPrpr $ C.PrprInclude C.Quotation2 $ T.pack headerFn

    addIfMissing x xs = if x `elem` xs then xs else x:xs

    headerFn :: FilePath
    headerFn  = nameStr prog ++ ".hpp"
    cppFn     = nameStr prog ++ "." ++ sourceExt
    sourceExt = case Native.language setup of
      Native.CPlusPlus -> "cpp"
      Native.CUDA      -> "cu"


  
