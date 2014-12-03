{-# LANGUAGE FlexibleContexts, TypeOperators, TupleSections #-}
-- repa semantics of the array language.

module Semantics where

import Control.Monad.Identity
import Expr
import Transformation
import Data.Ratio
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


visualize :: FilePath -> SimState U -> IO ()
visualize fn s = do
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

  writeImageToBMP fn (computeS bmp)






sround :: Rational -> Int
sround x
  | denominator x == 1 = round x
  | otherwise          = error $ "expected an integer, but got: " ++ show x

repaEval :: Expr Double -> SimState D -> Array D DIM3 Double
repaEval expr stat = ret
  where
    ret :: Array D DIM3 Double
    ret = fromFunction simExtent evalAt
    
    expr' =
      replaceAtom (Var "\\sigma")  (Static "\\sigma" sigma) $
      replaceAtom (Var "v")        (Static "v" velocity) $
      replaceAtom (Var "\\mu")     (Static "\\mu" mu) $
      replaceAtom (Var "\\lambda") (Static "\\lambda" lambda) $
      replaceAtom (Var "f")        (Static "f" extForce) $
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
    pt2ix3 f = ix3 (sround $ f X) (sround $ f Y) (sround $ f Z)

    statLookup :: String -> Pt -> Double
    statLookup varn pt = case M.lookup varn stat of
      Just ar -> ar ! pt2ix3 pt
      Nothing -> error $ "no such variable: " ++ varn

    sigma :: (Axis,Axis) -> Pt -> Double
    sigma (i,j) = statLookup $ "S" ++ concat (sort [show i, show j])

    velocity :: Axis -> Pt -> Double
    velocity i = statLookup $ "V" ++ show i

    extForce :: Axis -> Pt -> Double
    extForce _ _ = 0

    mu :: Pt -> Double
    mu = const 0.35
    lambda :: Pt -> Double
    lambda = const 1.0

-- "\\Delta \\sigma_{{x,x}}"
-- "\\Delta v_{x}"


mmapM :: (Ord k, Functor m, Monad m) => (a->m b) -> M.Map k a -> m (M.Map k b)
mmapM f map0 = fmap M.fromList $ mapM (\(k,v) -> fmap (k,) $ f v)  $ M.toList map0

proceed :: M.Map String (Expr Double) ->  SimState U -> IO (SimState U)
proceed eqMap state0 = do
  let state0D :: SimState D        
      state0D = M.map cbc state0

  state1 <- mmapM computeP $
    M.adjust (R.zipWith (+) $ repaEval (lookUpEqList "Vx") state0D) "Vx" $
    M.adjust (R.zipWith (+) $ repaEval (lookUpEqList "Vy") state0D) "Vy" $
    M.adjust (R.zipWith (+) $ repaEval (lookUpEqList "Vz") state0D) "Vz" $
    state0D

  let state1D :: SimState D
      state1D = M.map cbc (state1 :: SimState U)

  state2 <- mmapM computeP $ 
    M.insert "Sxx" (repaEval (lookUpEqList "Sxx") state1D) $
    M.insert "Syy" (repaEval (lookUpEqList "Syy") state1D) $
    M.insert "Szz" (repaEval (lookUpEqList "Szz") state1D) $
    M.insert "Sxy" (repaEval (lookUpEqList "Sxy") state1D) $
    M.insert "Sxz" (repaEval (lookUpEqList "Sxz") state1D) $
    M.insert "Syz" (repaEval (lookUpEqList "Syz") state1D) $
    state1D

  return state2

  where
    eqList = M.toList eqMap
  
    lookUpEqList :: String -> Expr Double
    lookUpEqList "Vx" = snd $ head $ filter (\(k,_) ->  "v_" `isInfixOf`k &&  "{x}"`isInfixOf`k) eqList
    lookUpEqList "Vy" = snd $ head $ filter (\(k,_) ->  "v_" `isInfixOf`k &&  "{y}"`isInfixOf`k) eqList
    lookUpEqList "Vz" = snd $ head $ filter (\(k,_) ->  "v_" `isInfixOf`k &&  "{z}"`isInfixOf`k) eqList
    lookUpEqList "Sxx" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{x,x}"`isInfixOf`k) eqList
    lookUpEqList "Syy" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{y,y}"`isInfixOf`k) eqList
    lookUpEqList "Szz" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{z,z}"`isInfixOf`k) eqList
    lookUpEqList "Sxy" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{x,y}"`isInfixOf`k) eqList
    lookUpEqList "Sxz" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{x,z}"`isInfixOf`k) eqList
    lookUpEqList "Syz" = snd $ head $ filter (\(k,_) ->  "sigma_" `isInfixOf`k &&  "{y,z}"`isInfixOf`k) eqList
  
