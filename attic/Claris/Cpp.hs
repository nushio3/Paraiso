{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Cpp ( 
  Config(..)
  ) where

import qualified Claris
import           Data.ListLike.String (StringLike)
import           Data.String (IsString)

data Config = Config

instance (StringLike text, IsString text) => Claris.Translator Config text where
  translate Config _ = "int main () {}"
  