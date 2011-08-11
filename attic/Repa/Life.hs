#!/usr/bin/env runhaskell
{-# Language FlexibleContexts, FlexibleInstances, FunctionalDependencies, 
MultiParamTypeClasses, OverloadedStrings, TypeOperators, UndecidableInstances #-}
{-# OPTIONS -Wall #-}

import           Data.Array.Repa ((:.)(..), (!))
import qualified Data.Array.Repa as R
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ListLike as LL
import qualified Data.ListLike.String as LL
import qualified Data.ListLike.Text as LL ()


ppCell :: R.Array R.DIM2 Bool -> T.Text
ppCell cell = ret
  where
    (R.Z :. w :. h) = R.extent cell
    
    ret = LL.unlines 
      [ LL.concat [ pat (cell ! (R.Z:.x:.y)) (cell ! (R.Z:.x:.(y+1)))
        | x <- [0 .. w-1]]
      | y <- [0 .. div h 2-1]]

    pat False False = " "
    pat False True  = "▄"
    pat True  False = "▀"
    pat True  True  = "█"
sh :: R.DIM2
sh = R.Z :. 60 :. 30

ar :: R.Array R.DIM2 Int
ar = R.fromFunction sh (\(R.Z :. x :. y) -> mod (x+y) 5)



main :: IO ()
main = do
  putStrLn $ R.showShape sh
  T.putStrLn $ ppCell $ R.map (/=0) ar
