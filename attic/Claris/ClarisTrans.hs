{-# LANGUAGE FlexibleContexts, ImpredicativeTypes, MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translatable(..), paren, joinBy, joinEndBy
  ) where

import           ClarisDef
import qualified Data.List as L
import qualified Data.ListLike as LL
import qualified Data.ListLike.String as LL
import           Util
import           Prelude hiding ((++))

class Translatable a where
  translate :: a -> Text

instance Translatable Program where
  translate Program{topLevel = xs} = LL.unlines $ map translate xs
  
instance Translatable TopLevel where  
  translate tl = case tl of
    PragmaDecl x -> translate x 
    FuncDecl   x -> translate x 
    UsingNamespaceStandard -> "using namespace std;"
    
instance Translatable Pragma where
  translate PragmaInclude {
    includeName = x, includeToHeader = _, includeParen = p
    } = "#include " ++ paren p x
        
  translate PragmaOnce = "#pragma once"

instance Translatable Function where
  translate _ = "TODO"
  
  
  
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

joinBy :: Text -> [Text] -> Text
joinBy sep xs = LL.concat $ L.intersperse sep xs
         
joinEndBy :: Text -> [Text] -> Text
joinEndBy sep xs = joinBy sep xs ++ sep

