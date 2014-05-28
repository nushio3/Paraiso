{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.OM.PrettyPrint (
  prettyPrint, prettyPrintA, prettyPrintA1
  ) where

import           Control.Monad
import qualified Data.Graph.Inductive                   as FGL
import           Data.List (sort)
import           Data.Maybe
import qualified Data.Set                               as Set
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import qualified Language.Paraiso.Annotation.Boundary   as Boundary
import qualified Language.Paraiso.Annotation.Dependency as Depend
import qualified Language.Paraiso.Annotation.Execution  as Exec
import           Language.Paraiso.Generator.ClarisTrans (dynamicDB)
import           Language.Paraiso.Interval
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Optimization.Graph    as Opt
import           Language.Paraiso.Prelude
import           Prelude hiding ((++))

-- | pretty print the OM, neglecting any annotations.
prettyPrint :: Opt.Ready v g => OM v g a -> T.Text
prettyPrint = prettyPrintA (const []) 

-- | pretty print the OM, using a default printing for annotation.
prettyPrintA1 :: Opt.Ready v g => OM v g Anot.Annotation -> T.Text
prettyPrintA1 om = prettyPrintA (ppAnot1 om) om

-- | pretty print the OM with your choice of prettyprinter for annotation.
prettyPrintA :: Opt.Ready v g => (a -> [T.Text]) -> OM v g a -> T.Text
prettyPrintA ppAnot om 
  = T.unlines 
    [ "OM name: " ++ nameText om,
      "** Static Variables",
      staticList,
      "** Kernels",
      kernelList
    ]
  where
    staticList = T.unlines $ V.toList $ V.map showT $ staticValues $ setup om
    kernelList = T.unlines $ V.toList $ V.map ppKern $ kernels om

    ppKern kern = T.unlines $ ["*** Kernel name: " ++ nameText kern] ++ concat (body (dataflow kern))
    body graph = map ppCon $ map (FGL.context graph) $ FGL.nodes graph

    ppCon (input, idx, nodeLabel, output) 
      = T.unwords
        [ showT idx, 
          ppNode nodeLabel,
          ppEdges "<-" input,
          ppEdges "->" output
        ] : ppAnot (getA nodeLabel)

    ppNode n = case n of
      NValue x          _ -> showT x
      NInst  (Imm dynX) _ -> 
        "Imm " ++ (fromJust $ dynamicDB dynX `mplus` Just (showT dynX))
      NInst  x          _ -> showT x

    ppEdges symbol xs 
      | length xs == 0 = ""
      | otherwise      = T.unwords $ symbol : map ppEdge (sort xs)
    ppEdge (e, i) = case e of
      EUnord -> showT i
      EOrd x -> "(" ++ showT x ++ ")" ++ showT i



ppAnot1 :: Opt.Ready v g => OM v g Anot.Annotation -> Anot.Annotation -> [T.Text]
ppAnot1 om anots = map ("  "++) $ concat cands
  where
    cands = 
      [ map showT ((Anot.toList anots) ::  [Alloc.Allocation])
      , map ppValid (toValidList om anots)      
      , map (("Depend."++) . showT) ((Anot.toList anots) ::  [Depend.Direct])
      , map (("Depend."++) . showT) ((Anot.toList anots) ::  [Depend.Indirect])
      , if ((Anot.toList anots) ::  [Alloc.Allocation]) == [Alloc.Manifest]
        then map (("Depend.Calc "++) . ppDC) ((Anot.toList anots) ::  [Depend.Calc])
        else []
      , map showT ((Anot.toList anots) ::  [Exec.Alive])
      , map showT ((Anot.toList anots) ::  [Depend.KernelWriteGroup])        
      , map showT ((Anot.toList anots) ::  [Depend.OMWriteGroup])                
      ]

    toValidList :: Opt.Ready v g => OM v g Anot.Annotation -> Anot.Annotation -> [Boundary.Valid g]      
    toValidList _ = Anot.toList

    ppValid (Boundary.Valid xs) = T.unwords $ map ppInterval xs
    ppInterval (Interval x y) 
      = ppNB x ++ ".." ++ ppNB y
    ppInterval Empty = "[empty]" 

    ppNB (Boundary.NegaInfinity)    = "[-inf"    
    ppNB (Boundary.LowerBoundary x) = "[" ++ showT x
    ppNB (Boundary.UpperBoundary x) = showT x ++ "]"
    ppNB (Boundary.PosiInfinity)    = "+inf]"

    ppDC (Depend.Calc s) = showT $ Set.toList s
