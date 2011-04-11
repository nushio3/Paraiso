module Language.Paraiso.Set (Set(..)) where
{-
In mathematics, a π-system on a set Ω is a set P, consisting of certain subsets of Ω, such that

    * P is non-empty.

    * A ∩ B ∈ P whenever A and B are in P.

That is, P is a non-empty family of subsets of Ω that is closed under finite intersections.
-}

class Set a where
  empty :: a
  null :: a -> Bool
  intersection :: a -> a -> a
  