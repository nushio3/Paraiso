{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.OM.PrettyPrint (
  prettyPrint, prettyPrintA
  ) where

import qualified Data.Graph.Inductive as FGL
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ListLike.String as LL
import qualified Data.ListLike.Text ()
import           Language.Paraiso.Name
import           Language.Paraiso.OM
import           Language.Paraiso.OM.Graph
import           Language.Paraiso.Prelude
import           Language.Paraiso.Tensor

prettyPrint :: (Vector v, Show (v g), Show a) => OM v g a -> T.Text
prettyPrint = prettyPrintA (const []) 

prettyPrintA :: (Vector v, Show (v g), Show a) => (a -> [T.Text]) -> OM v g a -> T.Text
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
      
    
    
