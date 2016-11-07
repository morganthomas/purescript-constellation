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
import Data.Monoid (class Monoid, mempty)

newtype ZipAlgebra a = ZipAlgebra a

instance zippableSemigroup :: (Zippable f, Semigroup g) => Semigroup (ZipAlgebra (f g)) where
  append (ZipAlgebra u) (ZipAlgebra v) = ZipAlgebra (zipWith append u v)

data ZipAlgebraWithUnit a = ZipUnit | ZipValue a

instance zippableSemigroupWithUnit :: (Zippable f, Semigroup g) => Semigroup (ZipAlgebraWithUnit (f g)) where
  append ZipUnit u = u
  append u ZipUnit = u
  append (ZipValue u) (ZipValue v) = ZipValue (zipWith append u v)
