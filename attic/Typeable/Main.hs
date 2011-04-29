{-# OPTIONS -Wall #-}

import Data.Dynamic
import Data.Maybe
import Data.Typeable

three :: Integer
three = 3

answer :: Double
answer = 42

msg :: String
msg = "abcde"

f :: Int -> Int
f = (*3)

assimilate :: (Typeable a) => a -> (TypeRep, Dynamic)
assimilate x = (typeOf x, toDyn x)


type Ex a = Dynamic -> a
ex :: (Typeable a) => Ex a
ex = fromJust.fromDynamic

materialize :: (TypeRep, Dynamic) -> String
materialize (typeRep, dyn) = 
  if typeRep == typeOf three 
  then show $ (ex::Ex Integer) dyn
  else if typeRep == typeOf answer
  then show $ (ex::Ex Double) dyn
  else if typeRep == typeOf "str"
  then (ex::Ex String) dyn
  else "?"

    


main :: IO ()
main = do
  putStrLn "hom mud"
  print $ typeOf (True,three,("Hogeee",answer))
  print $ typeOf f
  let xs = [assimilate three, assimilate answer, assimilate msg]
  print $ xs
  print $ map materialize xs

  
