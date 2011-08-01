{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translator(..)
  ) where

import           ClarisDef
import qualified Data.ListLike as LL
import           Util
import           Prelude hiding ((++))

class Translator config a where
  translate :: config -> a -> Text
  
  
parenthesize :: Text -> Text -> Text
parenthesize paren str = prefix ++ str ++ suffix
  where
    prefix = paren
    suffix = LL.reverse paren