#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import qualified Language.Paraiso.Tuning.Genetic as GA
import           System.Environment

main :: IO ()
main = do
  argv <- getArgs
  let dnas :: [GA.Genome]
      dnas = map (read . show) argv
  case dnas of
    [dna] -> do
      dna' <- GA.mutate dna
      print dna'
    [dna1, dna2] -> do
      dna' <- GA.cross dna1 dna2
      print dna'
    [dna1, dna2, dna3] -> do
      dna' <- GA.triangulate dna1 dna2 dna3
      print dna'
    _ -> do
         return ()
