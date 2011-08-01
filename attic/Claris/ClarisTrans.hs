{-# LANGUAGE FlexibleContexts, ImpredicativeTypes, MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translator(..), Parenthesis(..), paren, joinBy, joinEndBy
  ) where

import           ClarisDef
import qualified Data.List as L
import qualified Data.ListLike as LL
import           Data.ListLike.String (StringLike)
import           Data.String (IsString)
import           Util
import           Prelude hiding ((++))

class Translator config a where
  translate :: config -> a -> Text
  

data Parenthesis = Paren | Bracket | Brace 
                 | Chevron | Chevron2 | Chevron3 
                 | Quotation | Quotation2

-- | an parenthesizer for lazy person.
paren :: Parenthesis -> Text -> Text
paren p str = prefix ++ str ++ suffix
  where
    (prefix,suffix) = case p of
      Paren      -> ("(",")")
      Bracket    -> ("[","]")
      Brace      -> ("{","}")
      Chevron    -> ("<",">")
      Chevron2   -> ("<<",">>")
      Chevron3   -> ("<<<",">>>")
      Quotation  -> ("\'","\'")
      Quotation2 -> ("\"","\"")

joinBy :: (StringLike text, IsString text, LL.ListLike text char) 
          => text -> [text] -> text
joinBy sep xs = LL.concat $ L.intersperse sep xs
         
joinEndBy :: (StringLike text, IsString text, LL.ListLike text char) 
          => text -> [text] -> text
joinEndBy sep xs = joinBy sep xs ++ sep

