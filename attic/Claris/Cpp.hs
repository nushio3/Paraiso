{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Cpp ( 
  Config(..)
  ) where

import qualified Claris

data Config = Config

instance Claris.Translator Config where
  translate Config _ = "int main () {}"
  