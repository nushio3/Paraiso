#!/usr/bin/env runhaskell

import Data.List
import Text.Printf
import System.Process

import Expr
import Tensor
import Differential
import Transformation


compileStmts :: [Stmt (Pt->Double)] -> String
compileStmts xs = 
    printf "\\documentclass[9pt]{article}\\usepackage{breqn}\\begin{document}%s\\end{document}" $
    intercalate "\n\n" $ 
    map (printf "\\begin{dmath}%s\\end{dmath}" . show) $
    concat $ map compile xs
  where
    compile x = 
        map (bhs $ everywhere (usePartial4 :: Expr Double -> Expr Double))  $ 
        einsteinRule $ bhs (:$ r) x

    

main :: IO ()
main = do
  let i = Var "i" :: Expr Axis
      j = Var "j" :: Expr Axis

      r = Var "\\mathbf{r}" :: Expr Pt

      sigma = mkTF2 "\\sigma" 
      f = mkTF1 "f" 
      dV = mkTF1 "\\Delta v" 
      

      eqV :: Stmt (Pt -> Double)
      eqV = dV(i)   := (partial(j)(sigma(i,j))  + f(i) ) 



  writeFile "tmp.tex" $ compileStmts [eqV]    
  system "pdflatex tmp.tex"
  return ()
