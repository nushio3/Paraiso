{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, RankNTypes, 
StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
module Util (
  Text, (++), showT,
  
  Named(..),
  Nameable(..), namee
  ) where

import qualified Data.ListLike as LL
import           Data.ListLike.Text ()
import           Data.ListLike.String (toString)
import           Prelude hiding ((++))
import qualified Data.Text 

type Text = Data.Text.Text

showT :: Show a => a -> Data.Text.Text
showT = Data.Text.pack . show


infixr 5 ++

(++) :: forall full item .
        LL.ListLike full item =>
        full -> full -> full
(++) = LL.append


-- | something that has name.
class Nameable a where
  -- | get its name.
  name :: a -> Text
  -- | get its name as a 'String'.
  nameStr :: a -> String
  nameStr = toString . name


-- | Convert some type to a named type.
data Named a = Named Text a

instance Nameable (Named a) where
    name (Named n _) = n
instance Functor Named where
  fmap f (Named n a) = Named n $ f a


-- | The thing the name points to.
namee :: Named a -> a
namee (Named _ x) = x


deriving instance (Eq a) => Eq (Named a)
deriving instance (Ord a) => Ord (Named a)
deriving instance (Show a) => Show (Named a)
deriving instance (Read a) => Read (Named a)
