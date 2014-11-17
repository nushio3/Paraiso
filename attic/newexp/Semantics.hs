{-# LANGUAGE FlexibleContexts, TypeOperators #-}
-- repa semantics of the array language.

module Semantics where

import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import qualified Data.Map as M

type SimState r = M.Map String (Array r DIM3 Double)

simExtent = ix3 400 300 1

cbc :: (Source r Double) => Array r DIM3 Double -> Array D DIM3 Double
cbc ar = fromFunction simExtent (\(Z:.x:.y:.z) -> ar ! (Z:.(mod x sx):.(mod y sy):.(mod z sz)))
  where
    (Z:.sx:.sy:.sz) = simExtent

cbcState ::  SimState U -> SimState D
cbcState = M.map cbc 

initialState :: SimState U
initialState = M.fromList
  [ ("Vx", go (const 0))
  , ("Vy", go (const 0))
  , ("Vz", go (\(Z:.x:.y:.z) -> negate$ exp ((negate $ (fromIntegral $ x-200)**2+(fromIntegral $y-150)**2)/100)))
  , ("Sxx", go (const 0))
  , ("Syy", go (const 0))
  , ("Szz", go (const 0))
  , ("Sxy", go (const 0))
  , ("Sxz", go (const 0))
  , ("Syz", go (const 0))
  ]
  where go = computeS . fromFunction simExtent





visualize :: SimState U -> IO ()
visualize s = do
  let Just vz = M.lookup "Vz" s

      vz2D :: Array D DIM2 Double
      vz2D = slice vz (Any :. All :. All :. (0::Int))

      bround x = ret
        where
          ret 
            | x < (fromIntegral $ minBound `asTypeOf` ret) = minBound
            | x > (fromIntegral $ maxBound `asTypeOf` ret) = maxBound
            | otherwise                                    = round x

      bmp = R.map (\rz -> (255-bround (rz*300),255-bround (negate $ rz*300),255)) vz2D

  writeImageToBMP "test.bmp" (computeS bmp)


