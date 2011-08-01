{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Claris (
  Program(..), Text, Translator(..)
  ) where

import qualified Data.Text as Text

type Text = Text.Text

data Program = Program

class Translator config where
  translate :: config -> Program -> Text
  