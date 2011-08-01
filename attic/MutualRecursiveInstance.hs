#!/usr/bin/env runhaskell
data Black = BlackLeaf Int | BlackNode Red Red deriving (Eq, Show)
data Red = RedLeaf Double | RedNode Black Black deriving (Eq, Show)

class Incable a where
  inc :: a -> a

instance Incable Black where
  inc (BlackLeaf x) = BlackLeaf $ x+1
  inc (BlackNode a b) = BlackNode (inc a) (inc b)

instance Incable Red where
  inc (RedLeaf x) = RedLeaf $ x+1
  inc (RedNode a b) = RedNode (inc a) (inc b)

redBlack = BlackNode (RedLeaf 1.5) (RedNode (BlackLeaf 3) (BlackLeaf 1))

main = do
  print $ redBlack
  print $ inc redBlack
