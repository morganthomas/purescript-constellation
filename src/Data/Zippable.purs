-- Copyright 2016 Morgan Thomas
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--    http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Data.Zippable where

import Data.Functor (class Functor, map)
import Data.Array as A
import Data.List as L
import Data.List.Lazy as LL
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3, Tuple4, Tuple5)
import Control.Monad.List.Trans as LT
import Control.Monad (class Monad)

-- | The motivation for `Zippable2` is to describe container types where you can combine two
-- | instances of two related types (often the same type -- see `Zippable`) by applying a
-- | binary operation pointwise between the elements of the two instances to create a new
-- | structurally congruent instance.
-- | 
-- | `Zippable2`s must satisfy the following law, in addition to the relevant `Functor` laws:
-- |
-- | ```text
-- | zipWith (\x y -> f (g x) (h y)) v u = zipWith f (map g v) (map h u)
-- | ```
-- |
-- | `Zippable2` can be implemented whenever `f` implements `Traversable` and `g` implements
-- | `Foldable`; see https://wiki.haskell.org/Foldable_and_Traversable#Generalising_zipWith.
-- | This module does not include this generic implementation because of the feeling that
-- | the best implementation will depend on what `f` and `g` are. The instance arrayZippable2
-- | is a case in point; it uses Data.Array.zip, which has a native JavaScript implementation.
-- |
-- | I (Morgan Thomas) conjecture that although `Zippable2` can be implemented whenever `f`
-- | implements `Traversable` and `g` implements `Foldable`, the converse does not hold.
-- | That is, I conjecture that you cannot go from an implementation of `Zippable2 f g`
-- | to an implementation of `Traversable f` and `Foldable g`, even given that `f` and `g`
-- | are `Functor`s. This is the motivation for defining a separate type class for `Zippable2`.
class (Functor f, Functor g) <= Zippable2 f g where
  zipWith :: forall a b c. (a -> b -> c) -> (f a -> g b -> f c)

-- | A `Zippable` is a functor which is zippable with itself.
class (Zippable2 f f) <= Zippable f

instance arrayZippable2 :: Zippable2 Array Array where
  zipWith f a1 a2 = map (\(Tuple x y) -> f x y) (A.zip a1 a2)

instance arrayZippable :: Zippable Array

instance listZippable2 :: Zippable2 L.List L.List where
  zipWith = L.zipWith

instance listZippable :: Zippable L.List

instance listLazyZippable2 :: Zippable2 LL.List LL.List where
  zipWith = LL.zipWith

instance listLazyZippable :: Zippable LL.List

instance listTransformerZippable2 :: (Monad m) => Zippable2 (LT.ListT m) (LT.ListT m) where
  zipWith = LT.zipWith

instance listTransformerZippable :: (Monad m) => Zippable (LT.ListT m)

zip :: forall f g a b. (Zippable2 f g) => f a -> g b -> f (Tuple a b)
zip = zipWith Tuple

zip3 :: forall f a b c. (Zippable f) => f a -> f b -> f c -> f (Tuple3 a b c)
zip3 u v w = zipWith (\x (Tuple y z) -> tuple3 x y z) u (zip v w)

zip4 :: forall f a b c d. (Zippable f) => f a -> f b -> f c -> f d -> f (Tuple4 a b c d)
zip4 t u v w = zipWith Tuple t (zip3 u v w)

zip5 :: forall f a b c d e. (Zippable f) => f a -> f b -> f c -> f d -> f e -> f (Tuple5 a b c d e)
zip5 s t u v w = zipWith Tuple s (zip4 t u v w)
