{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, UnicodeSyntax #-}

-- The syntax is inspired by that of halide, and will allow expression of partial differential equations with much more ease.

import Data.List 
import Text.Printf
import Control.Monad.Writer
import System.Process

infix 0 ≔
(≔) :: (MonadWriter [String] m) => String -> String -> m ()
a ≔ b = tell $ [a ++ " &=& " ++ b ]

instance Num String where
  a+b = printf "%s+%s" a b
  a-b = printf "%s-%s" a b
  a*b = printf "%s %s" a b
  fromInteger = show

type Axis = String


δ :: [Axis] -> String
δ [i,j] = printf "\\delta_{%s,%s}" i j
δ _ = "tensor rank mismatch in δ"

ә :: [Axis] -> String -> String
ә [i] f = printf "\\partial_{%s}\\left(%s\\right)" i f
ә _   _ = "tensor rank mismatch in ә"

әt :: String -> String
әt f = printf "\\frac{\\partial {%s}}{\\partial t}"  f


σ :: [Axis] -> String
σ [i,j] = printf "\\sigma_{%s,%s}" i j
σ _ = "tensor rank mismatch in σ"

v :: [Axis] -> String
v [i] = printf "v_{%s}" i 
v _ = "tensor rank mismatch in v"

f :: [Axis] -> String
f [i] = printf "f_{%s}" i 
f _ = "tensor rank mismatch in f"

program :: Writer [String] ()
program = do
  let
      i = "i" :: Axis
      j = "j" :: Axis
      k = "k" :: Axis

      μ = "\\mu"
      λ = "\\lambda"

  әt(v[i])   ≔ ә[j](σ[i,j]+f[i])
  әt(σ[i,j]) ≔ μ * ә[j](v[j])
             + λ * (δ[i,j] * ә[k](v[k]))





main :: IO ()
main = do
  let progStr :: String 
      progStr = intercalate "\\\\" $ execWriter program
  writeFile "tmp.tex" $ printf 
    "\\documentclass{article}\\begin{document}\\begin{eqnarray}%s\\end{eqnarray}\\end{document}" progStr
  system "pdflatex tmp.tex"
  return ()