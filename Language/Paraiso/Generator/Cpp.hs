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
import qualified Data.Graph.Inductive as FGL
import qualified Data.List as List
import Data.Maybe
import qualified Data.Foldable as Foldable
import Language.Paraiso.Failure
import Language.Paraiso.Generator
import Language.Paraiso.OM.Arithmetic (arity)
import Language.Paraiso.OM.DynValue as DVal
import Language.Paraiso.OM.Graph
import Language.Paraiso.OM.Realm (Realm(..))
import qualified Language.Paraiso.OM.Reduce as Reduce
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
    writeFile (path </> cppFn) $ genCpp headerFn members pom

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
makeMembers pom =  [sizeMember] ++ sizeAMembers ++ map (CMember ReadWrite) vals 
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

genHeader :: (Vector v, Ring.C g) => [CMember] -> POM v g a -> String
genHeader members pom = unlines[
  commonInclude ,
  "class " ++ nameStr pom ++ "{",
  decStr,
  readerStr,
  writerStr,
  "public:",
  constructorStr,
  kernelStr,
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
      "const " ++ symbol Cpp dyn0 ++ " " ++ ref' ++ name1 ++ "() const { " ++ code name1 ++" }"
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

    initializer (Named name0 _) = let name1 = symbol Cpp name0 in
      name1 ++ "_(" ++ name1 ++ ")"
    initializeIfLocal (Named name0 dyn0) = let name1 = symbol Cpp name0 in
      if DVal.realm dyn0 == Global
      then []
      else [name1 ++ "_(" ++ symbol Cpp sizeName ++ "())"]
    initializerStr = concat $ List.intersperse "," $ concat $
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadInit  -> [initializer dv]
                            ReadWrite -> initializeIfLocal dv
                            _         -> [])
    cArg (Named name0 dyn0) = let name1 = symbol Cpp name0 in
      symbol Cpp dyn0 ++ " " ++ name1
    cArgStr = concat $ List.intersperse "," $ concat $
      (flip map) members $ 
      (\(CMember at dv) -> case at of
                            ReadInit -> [cArg dv]
                            _        -> [])
    constructorStr = nameStr pom ++ " ( " ++ cArgStr ++ " ): " 
                     ++ initializerStr ++ "{}"
    
    kernelStr = unlines $ map (\kernel -> "void " ++ nameStr kernel ++ " ();") $
                kernels pom

genCpp :: (Vector v, Ring.C g) => String -> [CMember] -> POM v g a -> String
genCpp headerFn _ pom = unlines [
  "#include \"" ++ headerFn ++ "\"",
  "",
  kernelsStr
                       ]
  where
    classPrefix = nameStr pom ++ "::"
    kernelsStr = unlines $ map (declareKernel classPrefix) $
                kernels pom


declareKernel :: (Vector v, Ring.C g) => String -> Kernel v g a -> String
declareKernel classPrefix kern = unlines [
  "void " ++ classPrefix ++ nameStr kern ++ " () {",
  declareNodes labNodes,
  genInsts labNodes,
  "return;",
  "}"
                     ]
  where
    graph = dataflow kern
    labNodes = FGL.labNodes graph

    nodeName n = "a" ++ show n
    nodeSinglet i n = nodeName n ++ i
    namedSinglet i name0 = symbol Cpp name0 ++ "()" ++ i

    nodeToRealm n = case (fromJust $ FGL.lab graph n) of
      NValue dyn0 _ -> DVal.realm dyn0
      _ -> error "nodeToRealm called on NInst"

    declareNodes = unlines . concat . map declareNode
    declareNode (n, node) = case node of
      NInst _ _  -> []
      NValue dyn0 _ -> [declareVal (nodeName n) dyn0]
    declareVal name0 dyn0 = let
      x = if DVal.realm dyn0 == Local 
          then "(" ++ symbol Cpp sizeName ++ "())"
          else ""
     in symbol Cpp dyn0 ++ " " ++ name0 ++ x ++ ";"

    genInsts = unlines . concat . map genInst
    genInst (n, node) =  case node of
      NValue _ _ -> []
      NInst inst _ -> [genInst' inst n (FGL.pre graph n) (FGL.suc graph n)]
    genInst' inst n pres sucs = let
      (np, ns) = arity inst
      suc0 = head sucs
      pre0 = head pres
      correct = np == length pres && ns == length sucs 
      comment = if correct then "" else "/* BAD ARITY */"
     in comment ++ case inst of
          Imm dyn0 -> env suc0 (\i -> nodeSinglet i suc0 ++ " = " ++ symbol Cpp dyn0 ++ ";")
          Load name0 -> env suc0 (\i -> nodeSinglet i suc0 ++ " = " ++ namedSinglet i name0 ++ ";")
          Store name0 -> env pre0 (\i -> namedSinglet i name0 ++ " = " ++ nodeSinglet i pre0 ++ ";")
          Reduce op -> envR op (nodeSinglet "" suc0) (\i -> nodeSinglet i pre0)
          Broadcast -> env suc0 (\i -> nodeSinglet i suc0 ++ " = " ++ nodeSinglet "" pre0 ++ ";")
          _ -> "/* noop */"

    env :: FGL.Node -> (String -> String) -> String
    env n f = if nodeToRealm n == Global
              then f ""
              else unlines ["for (int i = 0; i < " ++ nameStr sizeName ++ "() ; ++i) {", f "[i]","}"]
    envR op sum f =
      unlines [
        sum ++ " = " ++ f "[0];" ,
        "for (int i = 1; i < " ++ nameStr sizeName ++ "() ; ++i) {", 
        sum ++ " = " ++ genReduce op sum (f "[i]") ++ ";",
        "}"]
    genReduce op sum x = case op of
                           Reduce.Max -> "max(" ++ sum ++ "," ++ x ++ ")"
                           Reduce.Min -> "min(" ++ sum ++ "," ++ x ++ ")"
                           Reduce.Sum -> "(" ++ sum ++ "+" ++ x ++ ")"

commonInclude :: String
commonInclude = unlines[
                 "#include <vector>",
                 "#include <cmath>",
                 ""
                ]
