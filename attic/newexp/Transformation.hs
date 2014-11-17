{-# LANGUAGE RankNTypes #-}
module Transformation where

import Data.Dynamic

import Expr


-- partially apply as much staged computation as possible
stage :: Typeable a => Expr a -> Expr a
stage x = case x of
  (Reserved Pair :$ a :$ b) ->
    let a' = stage a
        b' = stage b
    in case (a', b') of
      (Static sa a0, Static sb b0) -> case cast (a0,b0) of
        Just ret -> reStatic ret
        Nothing  -> Reserved Pair :$  a' :$ b'
      _                            -> Reserved Pair :$  a' :$ b'

  (f :$ a) -> 
    let f' = stage f
        a' = stage a
    in case (f', a') of
      (Static sf f0, Static sa a0) -> reStatic (f0 a0)
      _                            -> f' :$ a'
  Op1 o f' a -> 
    let a' = stage a
    in case a' of
      (Static sa a0) -> reStatic (f' a0)
      _              -> Op1 o f' a'
  Op2 o f' a b -> 
    let a' = stage a
        b' = stage b
    in case (a', b') of
      (Static sa a0, Static sb b0) -> reStatic (f' a0 b0)
      _                            -> Op2 o f' a' b'

  _ -> x

  where
    reStatic val = Static (reppr val) val
  
    reppr :: forall a. Typeable a => a -> String
    reppr a = (show :: Double -> String) |||? const (ppr x) $ a


-- obtain the result of staged computation, if possible
runStatic :: Expr a -> Maybe a
runStatic (Static _ x) = Just x
runStatic _            = Nothing

runStaticEither :: Expr a -> Either String a
runStaticEither (Static _ x) = Right x
runStaticEither y            = Left $ "not static: " ++ ppr y

-- perform staged computation and obtain the result, if possible
evalStatic :: Typeable a => Expr a -> Maybe a
evalStatic = runStatic . stage
evalStaticEither :: Typeable a => Expr a -> Either String a
evalStaticEither = runStaticEither . stage


-- Apply the function at everywhere in an expresion
everywhere :: (Typeable a, Typeable b) => (Expr b->Expr b)->Expr a -> Expr a
everywhere f x = case x of
  (Op1 o g a)   -> f %? (Op1 o g (everywhere f a))
  (Op2 o g a b) -> f %? (Op2 o g (everywhere f a) (everywhere f b))
  (g :$ a)    -> f %? ((everywhere f g) :$ (everywhere f a))
  _           -> f %? x

-- Apply the polymorphic function at everywhere in an expresion, inside-out
everywhereP :: (Typeable a) => (forall b. Typeable b => Expr b->Expr b)->Expr a -> Expr a
everywhereP f x = case x of
  (Op1 o g a)   -> f (Op1 o g (everywhereP f a))
  (Op2 o g a b) -> f (Op2 o g (everywhereP f a) (everywhereP f b))
  (g :$ a)    -> f ((everywhereP f g) :$ (everywhereP f a))
  _           -> f x


-- Apply the polymorphic function at everywhere in an expresion, outside-in
everywhereP' :: (Typeable a) => (forall b. Typeable b => Expr b->Expr b)->Expr a -> Expr a
everywhereP' f x = let y = f x in case y of
  (Op1 o g a)   -> (Op1 o g (everywhereP' f a))
  (Op2 o g a b) -> (Op2 o g (everywhereP' f a) (everywhereP' f b))
  (g :$ a)    -> ((everywhereP' f g) :$ (everywhereP' f a))
  _           -> y


-- Apply the function at both hand sides of a statement
bhs :: (Expr a-> Expr b)-> Stmt a -> Stmt b
bhs f (lhs := rhs) = (f lhs := f rhs)


-- distribute function application among operators
distributeApply :: Typeable a => Expr a -> Expr a
distributeApply = everywhereP' distributeApply1

distributeApply1 :: Expr a -> Expr a
distributeApply1 x = case x of
  (Op1 o g a :$ y) -> (Op1 o (\a0 -> g (const a0) undefined) (a:$y) )
  (Op2 o g a b :$ y) -> (Op2 o (\a0 b0 -> g (const a0) (const b0) undefined) (a:$y) (b:$y) )
  _ -> x
