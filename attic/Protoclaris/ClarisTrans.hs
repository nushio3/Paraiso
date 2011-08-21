{-# LANGUAGE FlexibleContexts, ImpredicativeTypes, MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# OPTIONS -Wall #-}
module ClarisTrans (
  Translatable(..), paren, joinBy, joinEndBy
  ) where

import           ClarisDef
import           Control.Monad
import           Data.Dynamic
import qualified Data.List as L
import qualified Data.ListLike as LL
import qualified Data.ListLike.String as LL
import           Util
import           Prelude hiding ((++))

class Translatable a where
  translate :: a -> Text

instance Translatable Program where
  translate Program{topLevel = xs} = LL.unlines $ map translate xs

instance Translatable TopLevelElem where  
  translate tl = case tl of
    PragmaDecl x -> translate x 
    FuncDef   x -> translate x 
    UsingNamespace x -> "using namespace " ++ x ++ ";"

instance Translatable Pragma where
  translate PragmaInclude {
    includeName = x, includeToHeader = _, includeParen = p
    } = "#include " ++ paren p x

  translate PragmaOnce = "#pragma once"

instance Translatable Function where
  translate f = LL.unwords
    [ translate (funcType f)
    , funcName f
    , paren Paren $ joinBy ", " $ map (translate . StmtDecl) (funcArgs f)
    , paren Brace $ joinEndBy ";\n" $ map translate $ funcBody f]

instance Translatable Statement where    
  translate (StmtExpr x)             = translate x
  translate (StmtDecl (Var typ nam)) = LL.unwords [translate typ, nam]
  translate (StmtDeclInit v x)       = translate (StmtDecl v) ++ " = " ++ translate x
  translate (StmtReturn x)           = "return " ++ translate x
  translate StmtLoop                 = "todo"

instance Translatable TypeRep where  
  translate x = 
    case msum $ map ($x) typeRepDB of
      Just str -> str
      Nothing  -> error $ "cannot translate type: " ++ show x

instance Translatable Dynamic where  
  translate x = 
    case msum $ map ($x) dynamicDB of
      Just str -> str
      Nothing  -> error $ "cannot translate immediate of type: " ++ show x

instance Translatable Expr where
  translate expr = paren Paren ret
    where
      ret = case expr of
        (Imm x) -> translate x
        (VarExpr x) -> name x
        (FuncCall f args) -> (f++) $ paren Paren $ joinBy ", " $ map translate args
        (Op1Prefix op x) -> op ++ translate x
        (Op1Postfix op x) -> translate x ++ op
        (Op2Infix op x y) -> LL.unwords [translate x, op, translate y]
        (Op3Infix op1 op2 x y z) -> LL.unwords [translate x, op1, translate y, op2, translate z]
        
        
-- | The databeses for Haskell -> Cpp type name translations.
typeRepDB:: [TypeRep -> Maybe Text]
typeRepDB = map fst symbolDB

-- | The databeses for Haskell -> Cpp immediate values translations.
dynamicDB:: [Dynamic -> Maybe Text]
dynamicDB = map snd symbolDB

-- | The united database for translating Haskell types and immediate values to Cpp
symbolDB:: [(TypeRep -> Maybe Text, Dynamic -> Maybe Text)]
symbolDB = [ 
  add "void"          (\() -> ""),
  add "bool"          (\x->if x then "true" else "false"),
  add "int"           (showT::Int->Text), 
  add "long long int" (showT::Integer->Text), 
  add "float"         ((++"f").showT::Float->Text), 
  add "double"        (showT::Double->Text),
  add "std::string"   (showT::String->Text),
  add "std::string"   (showT::Text->Text)
       ]  
  where
    add ::  (Typeable a) => Text -> (a->Text) 
        -> (TypeRep -> Maybe Text,Dynamic -> Maybe Text)
    add = add' undefined
    add' :: (Typeable a) => a -> Text -> (a->Text) 
        -> (TypeRep -> Maybe Text,Dynamic -> Maybe Text)
    add' dummy typename f = 
      (\tr -> if tr==typeOf dummy then Just typename else Nothing,
       fmap f . fromDynamic)


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

