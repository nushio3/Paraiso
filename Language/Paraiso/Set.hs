module Language.Paraiso.Set (Set(..)) where
class Set a where
  empty :: a
  null :: a -> Bool
  intersection :: a -> a -> a
  