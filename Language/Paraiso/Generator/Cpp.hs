{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..)
    ) where
import qualified Algebra.Ring as Ring
import Control.Monad
import Data.Dynamic
import Language.Paraiso.Failure
import Language.Paraiso.Generator
import qualified Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm (Realm(..))
import Language.Paraiso.POM
import Language.Paraiso.Tensor
import NumericPrelude
import System.Directory
import System.FilePath

-- | The c++ code generator.
data Cpp = Cpp deriving (Eq, Show)

instance Generator Cpp where
  generate _ pom path = do
    let 
      apom = augument pom
      headerFn = nameStr apom ++ ".hpp"
      cppFn = nameStr apom ++ ".cpp"
    createDirectoryIfMissing True path
    writeFile (path </> headerFn) $ genHeader apom
    writeFile (path </> cppFn) $ genCpp apom

instance Symbolable Cpp Dynamic where
  symbolF Cpp dyn = let
    ret = msum $ map ($dyn) dynamicDB
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate symbol of type: " ++ show dyn
  
instance Symbolable Cpp TypeRep where
  symbolF Cpp tr = let
    ret = msum $ map ($tr) typeRepDB
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate type: " ++ show tr
  

instance Symbolable Cpp DVal.DynValue where
  symbolF Cpp dyn0 = do
    let
      container :: String -> String
      container = case DVal.realm dyn0 of
                    Global -> id
                    Local -> ("std::vector<"++).(++">")
    type0 <- symbolF Cpp $ DVal.typeRep dyn0
    return $ container type0

instance Symbolable Cpp Name where
  symbolF Cpp = return . nameStr
  

dynamicDB:: [Dynamic -> Maybe String]
dynamicDB = map fst symbolDB

typeRepDB:: [TypeRep -> Maybe String]
typeRepDB = map snd symbolDB

symbolDB:: [(Dynamic -> Maybe String, TypeRep -> Maybe String)]
symbolDB = [ 
     add "bool" (\x->if x then "true" else "false"),
     add "int" (show::Int->String), 
     add "long long int" (show::Integer->String), 
     add "float" ((++"f").show::Float->String), 
     add "double" (show::Double->String)
          ]  
  where
    add ::  (Typeable a) => String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add = add' undefined
    add' :: (Typeable a) => a -> String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add' dummy typename f = 
      (fmap f . fromDynamic, 
       \tr -> if tr==typeOf dummy then Just typename else Nothing)

augument :: (Vector v, Ring.C g) => POM v g a b -> POM v g a b
augument = id

genHeader, genCpp :: (Vector v, Ring.C g) => POM v g a1 a2 -> String
genHeader pom = unlines[
  commonInclude ,
  "class " ++ nameStr pom ++ "{",
  "public:",
  decStr,
  "};"
                ]
  where
    vals = staticValues $ setup pom
    declare (NamedValue name0 dyn0 _) =
      symbol Cpp dyn0 ++ " " ++ symbol Cpp name0 ++ ";"
    decStr = unlines $ map declare vals

genCpp    = const ""


commonInclude :: String
commonInclude = unlines[
                 "#include <vector>",
                 ""
                ]
