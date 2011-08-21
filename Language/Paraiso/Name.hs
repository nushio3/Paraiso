{-# Language StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | name identifier.
module Language.Paraiso.Name 
  (
   Name, Named(..), mkName,
   Nameable(..), namee
  ) where
import Control.Monad
import Data.Text (Text, unpack)

-- | a name.
newtype Name = Name Text deriving (Eq, Ord, Show, Read)

-- | create a name from a 'Text'. 
-- We do not export the constructor 'Name' for future extensibility.
mkName :: Text -> Name
mkName x = Name x

-- | something that has name.
class Nameable a where
  -- | get its name.
  name :: a -> Name
  -- | get its name as a 'Text'.
  nameText :: a -> Text
  nameText = (\(Name str) -> str) . name
  -- | get its name as a 'String'.
  nameStr :: a -> String
  nameStr = unpack . (\(Name str) -> str) . name

-- | 'Name' has 'Name'. 'Name' of 'Name' is 'Name' itself. 
instance Nameable Name where
  name = id

-- | Convert some type to a named type.
data Named a = Named Name a

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
