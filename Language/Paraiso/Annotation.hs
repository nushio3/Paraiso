{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Annotation
    (
     Annotation, empty, add, toList, toMaybe, map
    ) where

import Data.Dynamic
import Data.Maybe
import Language.Paraiso.Prelude hiding (map, toList)
import qualified Language.Paraiso.Prelude as P (map)

type Annotation = [Dynamic]

empty :: Annotation
empty = []

add :: (Typeable a) => a -> Annotation -> Annotation
add x ys = toDyn x : ys

toList :: (Typeable a) => Annotation -> [a]
toList =  catMaybes . P.map fromDynamic

toMaybe :: (Typeable a) => Annotation -> Maybe a
toMaybe = msum . P.map fromDynamic

map :: (Typeable a, Typeable b) => (a->b) -> Annotation -> Annotation
map f = P.map (maybeApply f)

maybeApply :: (Typeable a, Typeable b) => (a->b) -> Dynamic -> Dynamic
maybeApply f x =
    case dynApply (toDyn f) x of
      Just y  -> y
      Nothing -> x
