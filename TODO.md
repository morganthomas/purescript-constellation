 * Move instances into their respective packages when possible.
 * Refactor other instances of zip and related functions in the PureScript ecosystem to use
   Zippable. See: https://pursuit.purescript.org/search?q=zip
 * Figure out the right abstractions to capture things like Data.List.zipWithA and
   Control.Monad.List.Trans.zipWith'.
 * Prove that given `Zippable2 f g` you cannot derive `Traversable f` and `Foldable g`.  
