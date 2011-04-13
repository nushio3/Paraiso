module Language.Paraiso.PiSystem (PiSystem(..)) where
{- In mathematics, a pi-system is a non-empty family of sets that is closed
under finite intersections.  -}

class PiSystem a where
  empty :: a
  null :: a -> Bool
  intersection :: a -> a -> a
  
