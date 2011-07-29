{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | An effectless 'Annotation' with a comment 

module Language.Paraiso.Annotation.Comment
    (
     Comment(..)
    ) where

import Data.Dynamic
import Data.Text (Text)
import Language.Paraiso.Prelude

data Comment = Comment Text
             deriving (Eq, Show, Typeable)