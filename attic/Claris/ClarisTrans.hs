{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translator(..)
  ) where

import           ClarisDef
import           Data.ListLike.String (StringLike)
import           Data.String (IsString)

class (StringLike text, IsString text) => Translator config text where
  translate :: config -> Program -> text
  