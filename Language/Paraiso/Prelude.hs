{-# LANGUAGE CPP, NoImplicitPrelude, NoMonomorphismRestriction, RankNTypes #-}
{-# OPTIONS -Wall #-}
-- | an extension of the standard Prelude for paraiso.

module Language.Paraiso.Prelude
  (
   Boolean(..),
   Text, showT, 
   (++)) where
{-
import           Control.Applicative (Applicative(..), (<$>))
import           Control.Monad hiding 
    (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
-}

import           Data.ListLike (append)
import           Data.ListLike.Text ()
import qualified Data.ListLike.Base (ListLike)
import qualified Data.Text as Text
import qualified NumericPrelude as Prelude

import NumericPrelude hiding ((++), (||), (&&), not)

-- | An efficient String that is used thoroughout Paraiso modules.
type Text = Text.Text

showT :: Show a => a -> Text
showT = Text.pack . show

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
