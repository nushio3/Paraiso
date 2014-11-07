{-# LANGUAGE RankNTypes #-}
module Transformation where

import Data.Dynamic

import Expr


-- partially apply as much staged computation as possible
stage :: Expr a -> Expr a
stage x = case x of
  (f :$ a) -> 
    let f' = stage f
        a' = stage a
    in case (f', a') of
      (Static sf f0, Static sa a0) -> (Static (ppr x) (f0 a0))
      _                            -> f' :$ a'
  Op1 o f' a -> 
    let a' = stage a
    in case a' of
      (Static sa a0) -> Static (ppr x) (f' a0)
  Op2 o f' a b -> 
    let a' = stage a
        b' = stage b
    in case (a', b') of
      (Static sa a0, Static sb b0) -> Static (ppr x) (f' a0 b0)
  _ -> x

-- obtain the result of staged computation, if possible
runStatic :: Expr a -> Maybe a
runStatic (Static _ x) = Just x
runStatic _            = Nothing

runStaticEither :: Expr a -> Either String a
runStaticEither (Static _ x) = Right x
runStaticEither y            = Left $ "not static: " ++ ppr y

-- perform staged computation and obtain the result, if possible
evalStatic :: Expr a -> Maybe a
evalStatic = runStatic . stage
evalStaticEither :: Expr a -> Either String a
evalStaticEither = runStaticEither . stage


-- Apply the function at everywhere in an expresion
everywhere :: (Typeable a, Typeable b) => (Expr b->Expr b)->Expr a -> Expr a
everywhere f x = case x of
  (Op1 o g a)   -> f %? (Op1 o g (everywhere f a))
  (Op2 o g a b) -> f %? (Op2 o g (everywhere f a) (everywhere f b))
  (g :$ a)    -> f %? ((everywhere f g) :$ (everywhere f a))
  _           -> f %? x

-- Apply the polymorphic function at everywhere in an expresion
everywhereP :: (Typeable a) => (forall b. Typeable b => Expr b->Expr b)->Expr a -> Expr a
everywhereP f x = case x of
  (Op1 o g a)   -> f (Op1 o g (everywhereP f a))
  (Op2 o g a b) -> f (Op2 o g (everywhereP f a) (everywhereP f b))
  (g :$ a)    -> f ((everywhereP f g) :$ (everywhereP f a))
  _           -> f x


-- Apply the function at everywhere in a statement
everywhereS :: (Typeable a, Typeable b) => (Expr b->Expr b)-> Stmt a -> Stmt a
everywhereS f (lhs := rhs) = (everywhere f lhs := everywhere f rhs)


-- 
