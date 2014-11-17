{-# LANGUAGE TypeOperators #-}
-- repa semantics of the array language.

module Semantics where

import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import qualified Data.Map as M

type SimState = M.Map String (Array U DIM3 Double)

simExtent = ix3 400 300 1

initialState :: SimState
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


visualize :: SimState -> IO ()
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