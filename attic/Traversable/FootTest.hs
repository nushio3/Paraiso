{-# LANGUAGE TypeFamilies #-} 
{-OPTIONS -Wall #-}


data Duo a = Duo a a deriving(Eq,Ord,Read,Show)
data Trio a = Trio a a a deriving(Eq,Ord,Read,Show)

data Index2 = Index2 Int deriving(Eq,Ord,Read,Show)
data Index3 = Index3 Int deriving(Eq,Ord,Read,Show)



class Axis ax where
  type Vector ax :: * -> *
  getComponent :: ax -> Vector ax a -> a
  compose :: (ax -> a) -> Vector ax a

instance Axis Index2 where
  type Vector Index2 = Duo
  getComponent (Index2 i) (Duo a b)
    | i == 0 = a
    | i == 1 = b
    | True   = error "index out of bound"
  compose f = Duo (f (Index2 0)) (f (Index2 1))


instance Axis Index3 where
  type Vector Index3 = Trio
  getComponent (Index3 i) (Trio a b c)
    | i == 0 = a
    | i == 1 = b
    | i == 2 = c
    | True   = error "index out of bound"
  compose f = Trio (f (Index3 0)) (f (Index3 1)) (f (Index3 2))

ni :: Duo String
ni = Duo "core" "tsu"

san :: Trio String
san = compose (\ (Index3 i) -> show i)

-- san' = compose (\i -> getComponent i san)


main :: IO ()
main = do
  putStrLn "hawawa"
  print ni
  print san
