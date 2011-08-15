{-# LANGUAGE  FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
-- | a general code generator definition.
module Language.Paraiso.Generator
    (
     Generator(..)
    ) where

import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Generator.Claris      as C
import qualified Language.Paraiso.Generator.ClarisTrans as C
import qualified Language.Paraiso.Generator.Native      as Native
import qualified Language.Paraiso.Generator.OMTrans     as OM
import qualified Language.Paraiso.Generator.Plan        as Plan
import qualified Language.Paraiso.Generator.PlanTrans   as Plan
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM                    as OM
import qualified Language.Paraiso.Optimization          as Opt
import           Language.Paraiso.Prelude
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))


-- | The definition for library generator.
class Generator src where
  -- | Generate the library under a certain directory.
  generate 
    :: Native.Setup       -- ^The Library Setup. 
    -> src                -- ^The object from which the library shall be generated.
    -> [(FilePath, Text)] -- ^Returns the list of relative filenames and their contents to be generated.

  generateIO
    :: Native.Setup          -- ^The Library Setup. 
    -> src                   -- ^The object from which the library shall be generated.
    -> IO [(FilePath, Text)] -- ^The act of generation. Also returns the absolute path and contents of generated files.

  generateIO setup src = do
    let dir = Native.directory setup
    createDirectoryIfMissing True dir
    forM (generate setup src) $ \ (fn, con) -> do
      let absfn = dir </> fn
      T.writeFile absfn con
      return (absfn, con)

instance Generator C.Program where
  generate setup prog0 = 
    [ (headerFn, C.translate C.headerFile prog),
      (cppFn   , C.translate C.sourceFile prog)
    ]
    where
      headerFn :: FilePath
      headerFn  = nameStr prog ++ ".hpp"
      cppFn     = nameStr prog ++ "." ++ sourceExt
      sourceExt = case Native.language setup of
        Native.CPlusPlus -> "cpp"
        Native.CUDA      -> "cu"
      
      prog = prog0{C.topLevel = tlm}
      tlm0  = C.topLevel prog0

      pragmaOnce = C.Exclusive C.HeaderFile $ C.StmtPrpr $ C.PrprPragma "once"
      myHeader   = C.Exclusive C.SourceFile $ C.StmtPrpr $ C.PrprInclude C.Quotation2 $ T.pack headerFn
      
      tlm = addIfMissing pragmaOnce $ addIfMissing myHeader $ tlm0
      
      addIfMissing x xs = if x `elem` xs then xs else x:xs


instance (Opt.Ready v g) => Generator (Plan.Plan v g Anot.Annotation) where
  generate setup plan = generate setup $ Plan.translate setup plan
  
instance (Opt.Ready v g) => Generator (OM.OM v g Anot.Annotation) where 
  generate setup om = generate setup $ OM.translate setup om
