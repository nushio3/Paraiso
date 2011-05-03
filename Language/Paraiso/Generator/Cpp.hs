{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator.Cpp
    (
     module Language.Paraiso.Generator,
     Cpp(..)
    ) where
import Control.Monad
import Control.Monad.Failure
import Data.Dynamic
import Language.Paraiso.Generator
import System.Directory
import System.FilePath

data Cpp = Cpp deriving (Eq, Show)

instance Generator Cpp where
  generate _ pom path = do
    createDirectoryIfMissing True path
    writeFile (path </> "test.h") "tabula rasa\n"

instance Symbolable Cpp Dynamic where
  symbolF Cpp dyn = let
    f :: (Typeable a) => Dynamic -> Maybe a
    f = fromDynamic
    xs :: [Dynamic -> Maybe String]
    xs = [fmap (\x->if x then "true" else "false") . f,
          fmap (show::Int->String) . f, 
          fmap (show::Integer->String) . f, 
          fmap ((++"f").show::Float->String) . f, 
          fmap (show::Double->String) . f
          ]  
    ret = msum $ map ($dyn) xs
   in case ret of
      Just str -> return str
      Nothing -> failure $ StringException $ 
                 "Cpp cannot translate symbol of type " ++ show dyn
  