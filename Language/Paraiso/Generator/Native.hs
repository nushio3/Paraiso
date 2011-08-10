{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.Native (
  Language(..)
  ) where

data Language = CPlusPlus | CUDA
  deriving (Eq, Show)
