{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Cpp ( 
  Config(..)
  ) where

import qualified Claris
import           Data.ListLike (ListLike(..))

data Config = Config

instance (ListLike str char) => Claris.Translator Config str where
  translate Config _ = "int main () {}"
  