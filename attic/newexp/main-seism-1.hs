#!/usr/bin/env runhaskell

import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Printf
import System.Process
import Data.Array.Repa(ix3)

import Expr
import Tensor
import Differential
import Transformation
import Semantics

compileStmts :: [Stmt (Pt->Double)] -> String
compileStmts xs = 
    printf "\\documentclass{article}\\usepackage[a4paper,margin=1in,landscape]{geometry}\\usepackage{breqn}\\begin{document}%s\\end{document}" $
    intercalate "\n\n" $ 
    map (printf "\\begin{dmath}%s\\end{dmath}" . show) $
    concat $ map compile xs


compile :: Stmt (Pt->Double) -> [Stmt Double]
compile x = 
    map (bhs stage) $
    map (bhs $ everywhere (usePartial4 :: Expr Double -> Expr Double))  $ 
    einsteinRule $ 
    bhs distributeApply $
    bhs (:$ r) x
  where
    r = Var "\\mathbf{r}" :: Expr Pt

    

main :: IO ()
main = do
  let i = Var "i" :: Expr Axis
      j = Var "j" :: Expr Axis
      k = Var "k" :: Expr Axis

      σ = mkTF2 "\\sigma" 
      v = mkTF1 "v" 

      f = mkTF1 "f" 

      μ = mkTF0 "\\mu" 
      λ = mkTF0 "\\lambda" 

      dV = mkTF1 "\\Delta v" 
      dσ = mkTF2 "\\Delta \\sigma"
      
      eqV :: Stmt (Pt -> Double)
      eqS :: Stmt (Pt -> Double)
      
      eqV =       dV(i) := ә(j)(σ(i,j))  + f(i) 
      eqS =     dσ(i,j) := μ * (ә(i)(v(j)) + ә(j)(v(i)))
                         + λ * (δ(i,j) * ә(k)(v(k)))

      eqVdebug :: Stmt (Pt -> Double)
      eqVdebug =       dV(i) := v(i)


      eqS :: Stmt (Pt -> Double)
      eqS =     dσ(i,j) := μ * (ә(i)(v(j)) + ә(j)(v(i)))
                         + λ * (δ(i,j) * ә(k)(v(k)))


  writeFile "tmp.tex" $ compileStmts [eqV,eqS]    
  system "pdflatex tmp.tex"


  let eqMap :: M.Map String (Expr Double)
      eqMap =
        M.fromList $
        map (\(l:=r) -> (show l,r)) $
        concat $ map compile  [eqV,eqS]    

  print $map fst $M.toList $eqMap

  visualize "st0.bmp" initialState
  writeFile "st0.txt" $ show initialState
  
  let go s t = do
        visualize "tmp.bmp" s
        print t
        proceed eqMap s

  st2 <- foldM go initialState [0..99]
 
  visualize "st2.bmp" st2
  writeFile "st2.txt" $ show st2

--   mapM_ print $ map (\(l:=r) ->
--     repaEval (cbcState initialState) r (ix3 201 151 0)) $ compile eqS



  return ()
  
