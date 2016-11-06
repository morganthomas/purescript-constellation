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

module Data.Constellation where

import Data.Functor (class Functor)
import Data.Maybe (Maybe)

-- | Constellations must satisfy the following laws:
-- | 
-- | * (mapBinary f v u) == Nothing iff (structCong v u) == False.
-- | * If (structCong v u), then:
-- |   ```text
-- |   mapBinary (\x y -> f (g x) (h y)) v u == mapBinary f (fmap g v) (fmap h u)
-- |   ```
class (Functor f) <= Constellation f where
  structCong :: forall a b. f a -> f b -> Boolean
  mapBinary :: forall a b c. (a -> b -> c) -> (f a -> f b -> Maybe (f c))
