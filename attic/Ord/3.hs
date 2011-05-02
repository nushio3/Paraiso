{-# LANGUAGE FlexibleInstances, FunctionalDependencies, 
  MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- how do you create extra Eq while keeping access to original Prelude.Eq

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




class Eq a where
  type BoolFor a :: *
  (==) :: a -> a -> BoolFor a
  (/=) :: a -> a -> BoolFor a
  

instance (Prelude.Eq a) => Eq a where
  type  BoolFor a = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
  

newtype Hako a = Hako { unhako :: a } 


instance (Eq a, BoolFor a ~ Bool) => Eq (Hako a) where
  type BoolFor (Hako a) = Hako Bool
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
  
