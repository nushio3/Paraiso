{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Cpp ( 
  Config(..)
  ) where

import qualified Claris
import           Data.ListLike.String (StringLike(..))
import           Data.String (IsString(..))

data Config = Config

instance (StringLike str, IsString str) => Claris.Translator Config str where
  translate Config _ = "int main () {}"
  