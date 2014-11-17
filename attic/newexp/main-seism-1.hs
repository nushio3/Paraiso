#!/usr/bin/env runhaskell

import Data.List
import Text.Printf
import System.Process

import Expr
import Tensor
import Differential
import Transformation


main :: IO ()
main = do
  let i = Var "i" :: Expr Axis
      j = Var "j" :: Expr Axis

      r = Var "\\mathbf{r}" :: Expr Pt

      sigma = mkTF2 "\\sigma" 
      f = mkTF1 "f" 
      dV = mkTF1 "\\Delta v" 
      
      eqV' :: Stmt Double
      eqV' = dV(i) :$ r := (partial(j)(sigma(i,j)) :$ r) + (f(i) :$ r)





  let prog = map (everywhereS (usePartial4 :: Expr Double -> Expr Double))  $ einsteinRule $ eqV'

  mapM_ print $ prog

  writeFile "tmp.tex" $ 
    printf "\\documentclass[9pt]{article}\\usepackage{breqn}\\begin{document}%s\\end{document}" $
    intercalate "\n\n" $ 
    map (printf "\\begin{dmath}%s\\end{dmath}" . show) prog
  system "pdflatex tmp.tex"
  return ()
