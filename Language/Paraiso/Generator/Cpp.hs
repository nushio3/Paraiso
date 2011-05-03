{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..)
    ) where
import qualified Algebra.Ring as Ring
import Control.Monad as Monad
import Data.Dynamic
import qualified Data.List as List
import Data.Maybe
import qualified Data.Foldable as Foldable
import Language.Paraiso.Failure
import Language.Paraiso.Generator
import Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm (Realm(..))
import Language.Paraiso.POM as POM
import Language.Paraiso.Tensor
import NumericPrelude
import System.Directory
import System.FilePath

-- | The c++ code generator.
data Cpp = Cpp deriving (Eq, Show)

instance Generator Cpp where
  generate _ pom path = do
    let 
      members = makeMembers pom
      headerFn = nameStr pom ++ ".hpp"
      cppFn = nameStr pom ++ ".cpp"
    createDirectoryIfMissing True path
    writeFile (path </> headerFn) $ genHeader members pom
    writeFile (path </> cppFn) $ genCpp members pom

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


data AccessType = ReadWrite | ReadInit | ReadDepend String

data CMember = CMember {accessType :: AccessType, memberDV :: (Named DynValue)}

instance Nameable CMember where
  name = name . memberDV


sizeName :: Name
sizeName = Name "size"
sizeForAxis :: (Vector v) => Axis v -> Name
sizeForAxis axis = Name $ "size" ++ show (axisIndex axis)

makeMembers :: (Vector v, Ring.C g) => POM v g a -> [CMember]
makeMembers pom = map (CMember ReadWrite) vals ++ [sizeMember] ++ sizeAMembers
  where
    vals = staticValues $ POM.setup pom

    f :: (Vector v, Ring.C g) => POM v g a -> v CMember
    f _ = compose (\axis -> CMember ReadInit (Named (sizeForAxis axis) globalInt))

    sizeMember :: CMember
    sizeMember = CMember (ReadDepend $ "return " ++ prod ++ ";") (Named sizeName globalInt)
    globalInt = DynValue Global (typeOf (undefined::Int))

    sizeAMembers :: [CMember]
    sizeAMembers = Foldable.foldMap (:[]) $ f pom
    
    prod :: String
    prod = concat $ List.intersperse " * " $ map (\m -> nameStr m ++ "()") sizeAMembers

genHeader, genCpp :: (Vector v, Ring.C g) => [CMember] -> POM v g a -> String
genHeader members pom = unlines[
  commonInclude ,
  "class " ++ nameStr pom ++ "{",
  decStr,
  readerStr,
  writerStr,
  "};"
                ]
  where
    declare (Named name0 dyn0) =
      symbol Cpp dyn0 ++ " " ++ symbol Cpp name0 ++ "_;"
    decStr = unlines $ ("private:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadDepend _ -> []
                            _            -> [declare dv])

    reader (ref',code) (Named name0 dyn0) =
      let name1 = symbol Cpp name0 in
      "const " ++ symbol Cpp dyn0 ++ " " ++ ref' ++ name1 ++ "() { " ++ code name1 ++" }"
    readerCode n = "return " ++ n ++ "_ ;"
    readerStr = unlines $ ("public:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadDepend s -> [reader ("" ,const s)    dv]
                            _            -> [reader ("&",readerCode) dv])

    writer (ref',code) (Named name0 dyn0) =
      let name1 = symbol Cpp name0 in
      symbol Cpp dyn0 ++ " " ++ ref' ++ name1 ++ "() { " ++ code name1 ++" }"
    writerCode n = "return " ++ n ++ "_ ;"
    writerStr = unlines $ ("public:" :) $ concat $ 
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadWrite -> [writer ("&" ,writerCode) dv]
                            _         -> [])



genCpp _ _  = ""


commonInclude :: String
commonInclude = unlines[
                 "#include <vector>",
                 ""
                ]
