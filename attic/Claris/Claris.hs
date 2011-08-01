{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Claris (
  Program(..), Text, Translator(..)
  ) where

import qualified Data.Text as Text
import           Data.ListLike.String (StringLike(..))

type Text = Text.Text

data Program = Program

class (StringLike str) => Translator config str where
  translate :: config -> Program -> str
  