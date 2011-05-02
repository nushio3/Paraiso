{-# LANGUAGE FlexibleInstances, FunctionalDependencies, 
  MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- successful

import Prelude hiding (Eq(..), not, (&&), (||))
import qualified Prelude (Eq(..), not, (&&), (||))

infix  4  ==, /=
infixr 3  &&
infixr 2  ||

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


type family BoolFor a :: *

class Eq a where
  (==) :: a -> a -> BoolFor a
  (/=) :: a -> a -> BoolFor a
  
type instance BoolFor Double = Bool
instance Eq Double where
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
  

newtype Hako a = Hako { unhako :: a } 

type instance BoolFor (Hako a) = Hako Bool
instance (Eq a, BoolFor a ~ Bool) => Eq (Hako a) where
  (Hako a) == (Hako b) = Hako (a==b)
  (Hako a) /= (Hako b) = Hako (a==b)
               
main :: IO ()
main = do
  putStrLn "hi"
  let 
      ans :: Double
      ans = 42
  print $ 6*7==ans
  print $ 5*8==ans
  
