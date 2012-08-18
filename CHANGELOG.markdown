2.4
-----
* Added the indexed `Kleene` store to `Control.Lens.Internal`
* Moved `Gettable`, `Accessor`, `Settable` and `Mutator` to `Control.Lens.Internal`
* Added `cloneTraversal` to `Control.Lens.Traversal`
* Renamed `clone` to `cloneLens` in `Control.Lens.Type`
* Generalized the type of `zoom` to subsume `focus`.
* Removed `Focus(..)` from `Control.Lens.Type`.
* Factored out `Control.Lens.Isomorphic`.
* Moved many private types to `Control.Lens.Internal`
* Added `conFields` to `Language.Haskell.TH.Lens`.
* Added `System.FilePath.Lens`.

2.3
---
* Added missing `{-# INLINE #-}` pragmas
* Renamed `meanwhile` to `throughout` in `Control.Parallel.Strategies.Lens`
* Added `Magnify` to `Control.Lens.Getter`.
* Added `Zoom` to `Control.Lens.Type`.

2.2
---
* Added `<&=`, `<&~`, `<|=`, and `<|~`
* Moved `<>~`, `<<>~`, `<>=`, and `<<>=` to `Data.Monoid.Lens`
* Template Haskell now uses eager binding to avoid adding dependencies.

2.1
---
* Renamed `adjust` to `over`
* Added `au`, `auf` and `under`
* Added `Data.Monoid.Lens`
* Increased lower dependency bound on `mtl` for cleaner installation.
