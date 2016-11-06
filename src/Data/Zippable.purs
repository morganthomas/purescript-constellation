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
import Data.Tuple (Tuple(..))

-- | The motivation for `Zippable` is to describe container types where you can combine two
-- | instances of the type by applying a binary operation pointwise between the elements of
-- | the two instances to create a new structurally congruent instance.
-- | 
-- | `Zippable`s must satisfy the following laws, in addition to the `Functor` laws:
-- |
-- | ```text
-- | zip (\x y -> x) v u == v
-- | zip (\x y -> y) v u == u
-- | zip (\x y -> f (g x) (h y)) v u == zip f (map g v) (map h u)
-- | ```
class (Functor f) <= Zippable f where
  zip :: forall a b c. (a -> b -> c) -> (f a -> f b -> f c)

instance arrayZippable :: Zippable Array where
  zip f a1 a2 = map (\(Tuple x y) -> f x y) (A.zip a1 a2)

