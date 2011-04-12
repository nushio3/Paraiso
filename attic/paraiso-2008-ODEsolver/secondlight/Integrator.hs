{-# OPTIONS  -XFlexibleContexts #-}

module Integrator(
                  integrate1,
                  integrate4,
                  d_dt
)where

import Paraiso
import Debug
import Util
import Control.Monad


data ODE a = ODE (Expr a) (Expr a)
d_dt = ODE

type Integrator a = a->a->[ODE a]->Dsl ()


integrate1::(Register (Expr a),RealFrac a)=>Integrator a
integrate1 timestep timemax eqs = do 
  t <- allocate
  t =$ 0.0
  let
      dt = Imm timestep
      d_dt (ODE var expr) = do
        var =$ var + expr * dt
  sequential (round $ timemax/timestep::Int) $ do
    t =$ t + dt
    mapM_ d_dt eqs 

integrate4::(Register (Expr a),RealFrac a)=>Integrator a
integrate4 timestep timemax eqs = do 
  t <- allocate
  t =$ 0.0
  let 
    xs = map (\(ODE x e) -> x) eqs 
    exprs = map (\(ODE x e) -> e) eqs 
  x0s <- mapM (\_ -> allocate) xs
  v1s <- mapM (\_ -> allocate) xs
  v2s <- mapM (\_ -> allocate) xs
  v3s <- mapM (\_ -> allocate) xs
  v4s <- mapM (\_ -> allocate) xs
  let
      dt05 = Imm $ 0.5 * timestep
      dt   = Imm $ timestep
      dt16 = Imm $ 1/6 * timestep
           
  sequential (round $ timemax/timestep::Int) $ do
    zipWithM_ (\x x0 -> x0 =$ x) xs x0s
    zipWithM_ (\v1 e -> v1 =$ e) v1s exprs
    t =$ t + dt05
    zipWith3M_ (\x x0 v1 -> x =$ x0 + dt05 * v1) xs x0s v1s
    zipWithM_ (\v2 e -> v2 =$ e) v2s exprs
    zipWith3M_ (\x x0 v2 -> x =$ x0 + dt05 * v2) xs x0s v2s
    zipWithM_ (\v3 e -> v3 =$ e) v3s exprs
    t =$ t + dt05
    zipWith3M_ (\x x0 v3 -> x =$ x0 + dt * v3) xs x0s v3s
    zipWithM_ (\v4 e -> v4 =$ e) v4s exprs
    zipWith6M_ (\x x0 v1 v2 v3 v4 -> x =$ x0 + dt16*(v1 + 2*v2 + 2*v3 + v4))
              xs x0s v1s v2s v3s v4s
    