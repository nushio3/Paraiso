{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translator(..)
  ) where

import           ClarisDef

class Translator config where
  translate :: config -> Program -> Text
  