2.5
---
* Added `Control.Plated` -- a port of Neil Mitchell's `uniplate` that can be used on a `Traversal`.
* Added `Data.Data.Lens` with smart traversals that know how to avoid traversing parts of a structure that can't contain a given type!
* Added `Data.Typeable.Lens` with `_cast` and `_gcast` like `traverseData`
* Renamed `IndexedStore` to `Context` now that it is used in user-visible locations, and since I also use it as `uniplate`'s notion of a context.
* Added `Comonad` instances for `Context` and `Kleene`, so we can use stores as the notion of an editable context in uniplate
* Compatibility with both sets of template haskell quirks for GHC 7.6.1-rc1 and the GHC 7.6.1 development head.
* Renamed `children` to `branches` in `Data.Tree.Lens`.

2.4.0.2
-------
* GHC 7.6.1 development HEAD compatibility (but broke 7.6.1-rc1)

2.4.0.1
-------
* Haddock cleanup

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
