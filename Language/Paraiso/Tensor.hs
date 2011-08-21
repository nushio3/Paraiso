{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
 FunctionalDependencies, KindSignatures,
  MultiParamTypeClasses, NoImplicitPrelude,
  TypeOperators, UndecidableInstances  #-} 
{-# OPTIONS -Wall #-}
-- | A tensor algebra library. Main ingredients are :
-- 
-- 'Vec' and ':~' are data constructors for rank-1 tensor.
-- This is essentially a touple of objects of the same type.
-- 
-- 'Vector' is a class for rank-1 tensor.
--
-- 'Axis' is an object for accessing the tensor components.

module Language.Paraiso.Tensor
    (
     (:~)(..), Vec(..), Axis(..), (!),
     Vector(..), VectorRing(..),
     contract,
     Vec0, Vec1, Vec2, Vec3, Vec4
    ) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import           Language.Paraiso.Failure
import           Language.Paraiso.Prelude

infixl 9 !
-- | a component operator.
(!) :: Vector v => v a -> Axis v -> a
v ! i  = component i v   

-- | data constructor for 0-dimensional tensor.
data Vec a 
  = Vec 
  deriving (Eq, Ord, Show, Read)

-- | data constructor for constructing n+1-dimensional tensor
-- from n-dimensional tensor.
data (n :: * -> * ) :~ a 
  = (n a) :~ a 
  deriving (Eq, Show, Read)
infixl 3 :~

-- | the last component contributes the most to the ordering
instance (Ord (n a), Ord a) => Ord (n :~ a) where
  compare (xs :~ x) (ys :~ y) = compare (x, xs) (y, ys) 

instance Foldable Vec where
  foldMap = foldMapDefault
instance Functor Vec where
  fmap = fmapDefault
instance Traversable Vec where
  traverse _ Vec = pure Vec 
instance Applicative Vec where
  pure _  = Vec
  _ <*> _ = Vec

instance (Traversable n) => Foldable ((:~) n) where
  foldMap = foldMapDefault
instance (Traversable n) => Functor ((:~) n) where
  fmap = fmapDefault
instance (Traversable n) => Traversable ((:~) n) where
  traverse f (x :~ y) = (:~) <$> traverse f x <*> f y
instance (Applicative n, Traversable n) => Applicative ((:~) n) where
  pure x = pure x :~ x
  (vf :~ f) <*> (vx :~ x) = (vf <*> vx) :~ (f x)



-- | An coordinate 'Axis' , labeled by an integer. 
-- Axis also carries v, the container type for its corresponding
-- vector. Therefore, An axis of one type can access only vectors
-- of a fixed dimension, but of arbitrary type.
newtype (Vector v) => Axis v = Axis {axisIndex::Int} deriving (Eq,Ord,Show,Read)

-- | An object that allows component-wise access.
class (Traversable v) => Vector v where
  -- | Get a component within f, a context which allows 'Failure'.
  componentF :: (Failure StringException f) => 
                Axis v -- ^the axis of the component you want
                -> v a -- ^the target vector 
                -> f a -- ^the component, obtained within a 'Failure' monad

  -- | Get a component. This computation may result in a runtime error,
  -- though, as long as the 'Axis' is generated from library functions
  -- such as 'compose', there will be no error.
  component :: Axis v -> v a -> a
  component axis vec = unsafePerformFailure $ componentF axis vec
  -- | The dimension of the vector.
  dimension :: v a -> Int
  -- | Create a 'Vector' from a function that maps 
  -- axis to components.
  compose :: (Axis v -> a) -> v a

instance Vector Vec where
  componentF axis Vec 
    = failureString $ "axis out of bound: " ++ show axis
  dimension _ = 0
  compose _ = Vec 

instance (Vector v) => Vector ((:~) v) where
  componentF (Axis i) vx@(v :~ x) 
    | i==dimension vx - 1 = return x
    | True                = componentF (Axis i) v
  dimension (v :~ _) = 1 + dimension v
  compose f = let
    xs = compose (\(Axis i)->f (Axis i)) in xs :~ f (Axis (dimension xs))

-- | Vector whose components are additive is also additive.
instance (Additive.C a) => Additive.C (Vec a) where
  zero = compose $ const Additive.zero
  x+y  = compose (\i ->  x!i +  y!i)
  x-y  = compose (\i ->  x!i -  y!i)
  negate x = compose (\i -> negate $ x!i)

instance (Vector v, Additive.C a) => Additive.C ((:~) v a) where
  zero = compose $ const Additive.zero
  x+y  = compose (\i -> x!i + y!i)
  x-y  = compose (\i -> x!i - y!i)
  negate x = compose (\i -> negate $ x!i)

-- | Tensor contraction. Create a 'Vector' from a function that maps 
-- axis to component, then sums over the axis and returns @a@.
contract :: (Vector v, Additive.C a) => (Axis v -> a) -> a
contract f = foldl (+) Additive.zero (compose f)



-- | 'VectorRing' is a 'Vector' whose components belongs to 'Ring.C', 
-- thus providing unit vectors.
class  (Vector v, Ring.C a) => VectorRing v a where
  -- | A vector where 'Axis'th component is unity but others are zero.
  unitVectorF :: (Failure StringException f) => Axis v -> f (v a)
  -- | pure but unsafe version means of obtaining a 'unitVector'
  unitVector :: Axis v -> v a
  unitVector = unsafePerformFailure . unitVectorF

instance (Ring.C a) => VectorRing Vec a where
  unitVectorF axis
      = failureString $ "axis out of bound: " ++ show axis

instance (Ring.C a, VectorRing v a, Additive.C (v a)) 
    => VectorRing ((:~) v) a where
  unitVectorF axis@(Axis i) = ret
    where
      z = Additive.zero
      d = dimension z
      ret
        | i < 0 || i >= d   = failureString $ "axis out of bound: " ++ show axis
        | i == d-1          = return $ Additive.zero :~ Ring.one
        | 0 <= i && i < d-1 = liftM (:~ Additive.zero) $ unitVectorF (Axis i)
        | True              = return z 
        -- this last guard never matches, but needed to infer the type of z.

-- | Type synonyms
type Vec0 = Vec
type Vec1 = (:~) Vec0
type Vec2 = (:~) Vec1
type Vec3 = (:~) Vec2
type Vec4 = (:~) Vec3
