#!/usr/bin/env runhaskell

import Data.List
import Text.Printf
import System.Process

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
      eqV =       dV(i) := ә(j)(σ(i,j))  + f(i) 


      eqS =     dσ(i,j) := μ * (ә(i)(v(j)) + ә(j)(v(i)))
                         + λ * (δ(i,j) * ә(k)(v(k)))


  writeFile "tmp.tex" $ compileStmts [eqV,eqS]    
  system "pdflatex tmp.tex"

  mapM_ putStrLn $ map (\(l:=r) ->  debugPrint r) $ compile eqS

  visualize $ initialState

  return ()
