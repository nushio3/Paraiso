{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | 'Annotation' is a collection of 'Typeable's 
-- 

module Language.Paraiso.Annotation.Comment
    (
     Comment(..)
    ) where

import Data.Dynamic
import Data.Text (Text)

data Comment = Comment Text
             deriving (Eq, Show, Typeable)