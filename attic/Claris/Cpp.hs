{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Cpp ( 
  Config(..)
  ) where

import qualified Claris
import           Data.ListLike (ListLike(..))

data Config = Config

instance Claris.Translator Config where
  translate Config _ = "int main () {}"
  