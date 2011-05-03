{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..)
    ) where
import Control.Monad
import Data.Dynamic
import Language.Paraiso.Failure
import Language.Paraiso.Generator
import Language.Paraiso.OM.Graph
import System.Directory
import System.FilePath

data Cpp = Cpp deriving (Eq, Show)

instance Generator Cpp where
  generate _ pom path = do
    createDirectoryIfMissing True path
    writeFile (path </> "test.h") "he"

instance Symbolable Cpp Dynamic where
  symbolF Cpp dyn = let
    add' :: (Typeable a) => a -> String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add' dummy typename f = (fmap f . fromDynamic, undefined)
    add ::  (Typeable a) => String -> (a->String) 
        -> (Dynamic -> Maybe String, TypeRep -> Maybe String)
    add = add' undefined
    db :: [(Dynamic -> Maybe String, TypeRep -> Maybe String)]
    db = [ 
      add "bool" (\x->if x then "true" else "false"),
      add "int" (show::Int->String), 
      add "long long int" (show::Integer->String), 
      add "float" ((++"f").show::Float->String), 
      add "double" (show::Double->String)
          ]  
    dyndb = map fst db
    ret = msum $ map ($dyn) dyndb
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate symbol of type " ++ show dyn
  
instance Symbolable Cpp Name where
  symbolF Cpp = return . nameStr
  
  
  