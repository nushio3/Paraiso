{-# LANGUAGE TypeFamilies #-} 
{-# OPTIONS -Wall #-}


data Duo a = Duo a a deriving(Eq,Ord,Read,Show)
data Trio a = Trio a a a deriving(Eq,Ord,Read,Show)

data Index2 = Index2 Int deriving(Eq,Ord,Read,Show)
data Index3 = Index3 Int deriving(Eq,Ord,Read,Show)

data Axis v = Axis Int deriving(Eq,Ord,Read,Show)

class Vector v where
  getComponent :: Axis v -> v a -> a
  compose :: (Axis v -> a) -> v a

instance Vector Duo where
  getComponent (Axis i) (Duo a b)
    | i == 0 = a
    | i == 1 = b
    | True   = error "index out of bound"
  compose f = Duo (f (Axis 0)) (f (Axis 1))


instance Vector Trio where
  getComponent (Axis i) (Trio a b c)
    | i == 0 = a
    | i == 1 = b
    | i == 2 = c
    | True   = error "index out of bound"
  compose f = Trio (f (Axis 0)) (f (Axis 1)) (f (Axis 2))

ni :: Duo Int
ni = Duo 4 2

san :: Trio Int
san = Trio 3 9 8

san' = compose (\i -> getComponent i san)

kyu = compose (\i -> compose (\j ->  getComponent i san * getComponent j san))

roku = compose (\i -> compose (\j -> 
  concat $ replicate (getComponent i ni) $ show $ getComponent j san))

main :: IO ()
main = do
  putStrLn "hawawa"
  print ni
  print san
  print san'
  print kyu
  print roku
  