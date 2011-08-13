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
import           System.Posix.Unistd (usleep)

type Cell = R.Array R.DIM2 Int

ppCell :: R.Array R.DIM2 Bool -> T.Text
ppCell cell = ret
  where
    (R.Z :. w :. h) = R.extent cell
    
    ret = LL.unlines 
      [ LL.concat [ pat (cell ! (R.Z:.x:.y)) (cell ! (R.Z:.x:.(y+1)))
        | x <- [0 .. w-1]]
      | y <- [0, 2 .. 2*(div h 2) -1]]

    pat False False = " "
    pat False True  = "▄"
    pat True  False = "▀"
    pat True  True  = "█"




initAr :: R.DIM2 -> Cell
initAr sh = R.fromFunction sh (\x -> if elem x pat then 1 else 0)
  where
    origin = let (R.Z :. x :. y) = sh in R.Z :. (div x 2) :. (div y 2)
    pat = map (R.addDim origin) 
      [ R.Z :. 1 :. 0 ,
        R.Z :. 2 :. 0 ,
        R.Z :. 0 :. 1 ,
        R.Z :. 1 :. 1 ,
        R.Z :. 1 :. 2 
      ]

proceed :: Cell -> Cell
proceed cell = nextCell
  where
    nextCell = R.fromFunction world next
    
    next i 
      | get i == 0 && nbd i == 3                  = 1
      | get i == 1 && (nbd i == 2 || nbd i ==3)   = 1
      | otherwise                                 = 0
        
    world = R.extent cell
    get i = case R.safeIndex cell i of
      Just x  -> x
      Nothing -> 0

    nbd i = sum $ map (get . R.addDim i) adjs

    adjs = 
      [ R.Z :. -1 :. -1,
        R.Z :.  0 :. -1,
        R.Z :.  1 :. -1,
        R.Z :. -1 :.  0,
        R.Z :.  1 :.  0,
        R.Z :. -1 :.  1,
        R.Z :.  0 :.  1,
        R.Z :.  1 :.  1
      ]

main :: IO ()
main = do
  loop (initAr $ R.Z :. 128 :. 128)


loop :: Cell -> IO ()
loop cell = do
  T.putStrLn $ ppCell $ R.map (/=0) cell 
  usleep 10000
  loop (R.force $ proceed cell)
