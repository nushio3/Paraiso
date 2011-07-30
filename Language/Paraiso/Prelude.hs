{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, RankNTypes #-}
{-# OPTIONS -Wall #-}
{- | 
   Redefine some items from the standard Prelude.
-}

module Language.Paraiso.Prelude
  (
   module Control.Applicative,
   module Control.Monad,
   module Data.Foldable,
   module Data.Traversable,
   module NumericPrelude,
   Boolean(..),
   (++)) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Monad hiding 
    (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import           Data.Foldable
import           Data.ListLike (append)
import qualified Data.ListLike.Base (ListLike)
import           Data.Traversable
import           NumericPrelude hiding 
    (not, (&&), (||), Monad, Functor, (*>), (++),
     (>>=), (>>), return, fail, fmap, mapM, mapM_, sequence, sequence_, 
     (=<<), foldl, foldl1, foldr, foldr1, and, or, any, all, sum, product, 
     concat, concatMap, maximum, minimum, elem, notElem)
import qualified NumericPrelude as Prelude

infixr 3  &&
infixr 2  ||
infixr 5  ++

(++) :: forall full item .
        Data.ListLike.Base.ListLike full item =>
        full -> full -> full
                                                                     
(++) = append

class Boolean b where
  true, false :: b
  not         :: b -> b
  (&&), (||)  :: b -> b -> b

instance Boolean Bool where
  true  = True
  false = False
  not   = Prelude.not
  (&&)  = (Prelude.&&)
  (||)  = (Prelude.||)
