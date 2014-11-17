{-# LANGUAGE FlexibleContexts, TypeOperators #-}
-- repa semantics of the array language.

module Semantics where

import Expr
import Transformation

import Data.Array.Repa as R hiding (Z,(++))
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import Data.List
import qualified Data.Map as M

type SimState r = M.Map String (Array r DIM3 Double)

simExtent = ix3 400 300 1

cbc :: (Source r Double) => Array r DIM3 Double -> Array D DIM3 Double
cbc ar = fromFunction simExtent (\(R.Z:.x:.y:.z) -> ar ! ix3 (mod x sx) (mod y sy)(mod z sz))
  where
    (R.Z:.sx:.sy:.sz) = simExtent

cbcState ::  SimState U -> SimState D
cbcState = M.map cbc 

initialState :: SimState U
initialState = M.fromList
  [ ("Vx", go (const 0))
  , ("Vy", go (const 0))
  , ("Vz", go (\(R.Z:.x:.y:.z) -> negate$ exp ((negate $ (fromIntegral $ x-200)**2+(fromIntegral $y-150)**2)/100)))
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








repaEval :: SimState D -> Expr Double -> IO (Array U DIM3 Double)
repaEval stat expr = computeUnboxedP ret
  where
    ret :: Array D DIM3 Double
    ret = fromFunction simExtent evalAt
    
    expr' =
      replaceAtom (Var "\\sigma")  (Static "!" sigma) $
      replaceAtom (Var "v")        (Static "!" velocity) $
      replaceAtom (Var "\\mu")     (Static "!" mu) $
      replaceAtom (Var "\\lambda") (Static "!" lambda) $
      expr

    evalAt :: DIM3 -> Double
    evalAt pt0 = ret0
      where
        ret0 :: Double
        ret0 = case evalStaticEither expr'' of
          Right x -> x
          Left err -> error err
        
        (R.Z:.px0:.py0:.pz0) = pt0
        
        ptFun :: Pt 
        ptFun X = fromIntegral px0
        ptFun Y = fromIntegral py0
        ptFun Z = fromIntegral pz0

        expr'' = replaceAtom (Var "\\mathbf{r}")  (Static "!" ptFun) $ expr'


    pt2ix3 :: Pt -> DIM3
    pt2ix3 f = ix3 (floor $ f X) (floor $ f Y) (floor $ f Z)

    statLookup :: String -> Pt -> Double
    statLookup varn pt = case M.lookup varn stat of
      Just ar -> ar ! pt2ix3 pt
      Nothing -> error $ "no such variable: " ++ varn

    sigma :: (Axis,Axis) -> Pt -> Double
    sigma (i,j) = statLookup $ "S" ++ concat (sort [show i, show j])

    velocity :: Axis -> Pt -> Double
    velocity i = statLookup $ "V" ++ show i

    mu :: Pt -> Double
    mu = const 0.35
    lambda :: Pt -> Double
    lambda = const 1.0


