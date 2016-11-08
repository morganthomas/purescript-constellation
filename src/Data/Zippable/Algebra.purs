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

module Data.Zippable.Algebra where

import Data.Semigroup (class Semigroup, append)
import Data.Zippable (class Zippable, zipWith)
import Data.Monoid (class Monoid)
import Data.Group (class Group, ginverse)
import Data.Functor (map)

newtype ZipAlgebra a = ZipAlgebra a

instance zippableSemigroup :: (Zippable f, Semigroup g) => Semigroup (ZipAlgebra (f g)) where
  append (ZipAlgebra u) (ZipAlgebra v) = ZipAlgebra (zipWith append u v)

-- | These exist to solve a conceptual problem. Consider the case Array Number. There is no
-- | identity element for the hypothetical Monoid this would form as a zip algebra, because
-- | each number array filled with zeroes acts as identity element only with arrays of the
-- | same length. (This kind of problem technically violates most algebraic axioms, but
-- | the axioms will hold when you're only dealing with number arrays of a single length, which
-- | can be good enough in practice.) In the case of the identity element it's a bigger issue
-- | because it actually violates the type class interface for Monoid if there's no single,
-- | working identity element. The ZipAlgebraWithUnit type simply adds an artifical unit value
-- | to any type you give it, which can act as a true identity element.
data ZipAlgebraWithUnit a = ZipUnit | ZipValue a

instance zippableSemigroupWithUnit :: (Zippable f, Semigroup g) => Semigroup (ZipAlgebraWithUnit (f g)) where
  append ZipUnit u = u
  append u ZipUnit = u
  append (ZipValue u) (ZipValue v) = ZipValue (zipWith append u v)

instance zippableMonoid :: (Zippable f, Monoid m) => Monoid (ZipAlgebraWithUnit (f m)) where
  mempty = ZipUnit

instance zippableGroup :: (Zippable f, Group g) => Group (ZipAlgebraWithUnit (f g)) where
  ginverse ZipUnit = ZipUnit
  ginverse (ZipValue v) = ZipValue (map ginverse v)
