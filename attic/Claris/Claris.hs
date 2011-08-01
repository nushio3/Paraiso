{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Claris (
  Program(..), Translator(..)
  ) where

import Data.ListLike (ListLike(..))

data Program = Program

class (ListLike str char) => Translator config str where
  translate :: config -> Program -> str
  