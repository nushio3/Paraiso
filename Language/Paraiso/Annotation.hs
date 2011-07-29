{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
-- | 'Annotation' is a collection of 'Typeable's 
-- with which you can annotate each OM node.

module Language.Paraiso.Annotation
    (
     Annotation, empty, add, toList, toMaybe, map
    ) where

import Data.Dynamic
import Data.Maybe
import Language.Paraiso.Prelude hiding (map, toList)
import qualified Language.Paraiso.Prelude as P (map)

type Annotation = [Dynamic]

-- | An empty collection.
empty :: Annotation
empty = []

-- | Add an annotation to a collection.
add :: (Typeable a) => a -> Annotation -> Annotation
add x ys = toDyn x : ys

-- | Extract all annotations of type @a@ from 
-- the collection.
toList :: (Typeable a) => Annotation -> [a]
toList =  catMaybes . P.map fromDynamic

-- | Extract the first annotation of the given type,
-- if it exists.
toMaybe :: (Typeable a) => Annotation -> Maybe a
toMaybe = msum . P.map fromDynamic

-- | Map all annotations of type @a@ to type @b@,
-- while leaving the others untouched.
map :: (Typeable a, Typeable b) => (a->b) -> Annotation -> Annotation
map f = P.map (maybeApply f)

maybeApply :: (Typeable a, Typeable b) => (a->b) -> Dynamic -> Dynamic
maybeApply f x =
    case dynApply (toDyn f) x of
      Just y  -> y
      Nothing -> x
