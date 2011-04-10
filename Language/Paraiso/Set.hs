module Language.Paraiso.Set (Set(..)) where
class Set a where
  empty :: a
  intersection :: a -> a -> a
  