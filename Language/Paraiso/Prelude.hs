{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
{- |
   Redefine some items from the standard Prelude.
-}


module Language.Paraiso.Prelude
  (module NumericPrelude,
   Boolean(..)) where

import NumericPrelude hiding (not, (&&), (||))
import qualified NumericPrelude as Prelude

infixr 3  &&
infixr 2  ||

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
