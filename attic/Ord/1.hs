{-# LANGUAGE FlexibleInstances, FunctionalDependencies, 
  MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- this doesn't work

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

class Eq b a | a->b where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b
  
  
instance Prelude.Eq a => Eq Bool a where
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
  
newtype Hako a = Hako { unhako :: a } 

instance Eq Bool a => Eq (Hako Bool) (Hako a) where
  (Hako a) == (Hako b) = Hako (a==b)
  (Hako a) /= (Hako b) = Hako (a==b)
               
main :: IO ()
main = do
  putStrLn "hi"
  let 
      ans :: Double
      ans = 42
  print $ 6*7==ans
  print $ if 5*8==ans 
     then "the world is at crysis"
     else "the world is saved"
  
