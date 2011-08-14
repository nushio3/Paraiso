{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.OM.PrettyPrint (
  prettyPrint, prettyPrintA, prettyPrintA1
  ) where

import qualified Data.Graph.Inductive as FGL
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ListLike.String as LL
import qualified Data.ListLike.Text ()
import qualified Language.Paraiso.Annotation as Anot
import qualified Language.Paraiso.Annotation.Allocation as Anot
import qualified Language.Paraiso.Annotation.Boundary as Anot
import           Language.Paraiso.Interval
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

-- | pretty print the OM, neglecting any annotations.
prettyPrint :: (Vector v, Show (v g)) => OM v g a -> T.Text
prettyPrint = prettyPrintA (const []) 

-- | pretty print the OM, using a default printing for annotation.
prettyPrintA1 :: (Vector v, Show (v g)) => OM v g Anot.Annotation -> T.Text
prettyPrintA1 = prettyPrintA ppAnot1

-- | pretty print the OM with your choice of prettyprinter for annotation.
prettyPrintA :: (Vector v, Show (v g)) => (a -> [T.Text]) -> OM v g a -> T.Text
prettyPrintA ppAnot om 
  = LL.unlines 
    [ "OM name: " ++ nameText om,
      "** Static Variables",
      staticList,
      "** Kernels",
      kernelList
    ]
  where
    staticList = LL.unlines $ V.toList $ V.map showT $ staticValues $ setup om
    kernelList = LL.unlines $ V.toList $ V.map ppKern $ kernels om

    ppKern kern = LL.unlines $ ["*** Kernel name: " ++ nameText kern] ++ concat (body (dataflow kern))
    body graph = map ppCon $ map (FGL.context graph) $ FGL.nodes graph

    ppCon (input, idx, nodeLabel, output) 
      = LL.unwords
        [ showT idx, 
          ppNode nodeLabel,
          ppEdges "<-" input,
          ppEdges "->" output
        ] : ppAnot (getA nodeLabel)

    ppNode n = case n of
      NValue x _ -> showT x
      NInst  x _ -> showT x

    ppEdges symbol xs 
      | length xs == 0 = ""
      | otherwise      = LL.unwords $ symbol : map ppEdge (sort xs)
    ppEdge (e, i) = case e of
      EUnord -> showT i
      EOrd x -> "(" ++ showT x ++ ")" ++ showT i


ppAnot1 :: Anot.Annotation -> [T.Text]
ppAnot1 anots = map ("  "++) $ concat cands
  where
    cands = 
      [ map showT ((Anot.toList anots) :: [Anot.Allocation])
      , map ppValid ((Anot.toList anots) :: [Anot.Valid Int])
      ]
      
    ppValid (Anot.Valid xs) = LL.unwords $ map ppInterval xs
    ppInterval (Interval x y) 
      = ppNB x ++ ".." ++ ppNB y
    ppInterval Empty = "[empty]" 
    
    ppNB Anot.NegaInfinity = "[-inf"    
    ppNB (Anot.LowerBoundary x) = "[" ++ showT x
    ppNB (Anot.UpperBoundary x) = showT x ++ "]"
    ppNB Anot.PosiInfinity = "+inf]"