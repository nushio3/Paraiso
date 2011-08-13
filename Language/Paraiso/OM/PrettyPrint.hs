{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.OM.PrettyPrint (
  prettyPrint
  ) where

import qualified Data.Graph.Inductive as FGL
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
prettyPrint om = LL.unlines [nameText om,staticList,kernelList]
  where
    staticList = LL.unlines $ V.toList $ V.map showT $ staticValues $ setup om
    kernelList = LL.unlines $ V.toList $ V.map ppKern $ kernels om

    ppKern kern = LL.unlines $ ["", "**** " ++ nameText kern ++ " ****"] ++ body (dataflow kern)
    body graph = map ppCon $ map (FGL.context graph) $ FGL.nodes graph
    
    ppCon (input, idx, nodeLabel, output) 
      = LL.unwords
        [ showT idx, 
          showT nodeLabel,
          "<-",
          ppEdges input,
          "->",
          ppEdges output
        ]
    ppEdges = LL.unwords . map ppEdge 
    ppEdge (e, i) = case e of
      EUnord -> showT i
      EOrd x -> "(" ++ showT x ++ ")" ++ showT i
      
    
    
