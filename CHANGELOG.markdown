3.5.1
-----
* Improved SafeHaskell inference.

3.5
---
* Fixed a potential SafeHaskell issue where a user could use `undefined` to derive `unsafeCoerce`. You now have to import an explicitly
  Unsafe module and create an instance of `Trustworthy` for your type to cause this behavior, so if you do, its on your head, not mine. :)
* Renamed `EvilBazaar` to `BazaarT`.
* Moved a lot of internals around. Most notably, `Gettable`, `Settable` and `Effective` have moved to `Control.Lens.Classes`.
* Exposed `partsOf'` and `unsafePartsOf'` in `Control.Lens.Traversal` to reduce reliance on `BazaarT` in `Control.Lens.Zipper`

3.4
---
* Renamed `(%)` to `(&)` and `(^%)` to `(^&)`. This avoids the conflict with `Data.Ratio`, which was our highest priority conflict with a third party library.
* Switched to a more liberal type for `ignored`
* Removed some "`isplitting`" bad combinators from `Control.Lens.IndexedFold`.
* Made `indexed`, `taking`, and `dropping` and `elementOf` lazier and capable of dealing with infinite traversals and infinite folds.
* Improved `Indexing` to support infinite traversals and folds.
* Removed some of the more redundant combinators from `Control.Lens.Plated`, which already had existing aliases in the rest of the traversal API.
* Moved `partsOf`, `holesOf`, and `elementOf` into `Control.Lens.Traversal`.
* Renamed `query` to `peruse` and `queries` to `peruses`. These are much less contentious names,
  both contain `use` in their name for analogy to `use` and `uses` and the word is about reading.
* Simpler `simple`.
* Added `enum` and `non` to `Control.Lens.Iso`.
* Added `(^?!)` to `Control.Lens.Fold` for unsafe access to the head of a `Fold`.
* Changed `_head`, `_tail`, `_init` and `_last` to traversals in `Data.List.Lens` and `Data.Sequence.Lens`.
* Eliminated `traverseHead`, `traverseTail`, `traverseInit` and `traverseLast`.
* `partsOf` and `unsafePartsOf` can now also be applied to a `Fold` yielding a `Getter` or to a `MonadicFold` yielding an `Action`.

3.3
---
* Redefined `simple` and moved it to `Control.Lens.Iso`. Instead of using `simple l` you can now compose `l.simple` or `simple.l` providing more nuanced control and a more compositional API.
* Moved the various `foo#` combinators used to emit cleaner core into an unexported module, `Control.Lens.Unsafe`. This removes `MagicHash` from the public API.
* Removed the `bazaar#` and `runBazaar#` coercions that caused issues on GHC HEAD.
* Changed the default definition of `plate` to `uniplate` from `ignored`.
* Added `Data.Vector.Lens` and instances for `Data.Vector`.
* Added support for the `split` package, which is now part of the Haskell platform.
* Removed redundant `Data.List.traverseList`. Use `itraversed` or `traverse` instead.
* Moved `(:<->)` to `Control.Lens.Simple`.
* Fixed a bug in `Control.Lens.TH` that was causing `makeIso` not to work.
* Added `lifted` to `Control.Lens.Setter` for mapping over monads.
* Added `beside` to `Control.Lens.Traversal`.
* Removed the operators from `Data.List.Lens`, they broke the overall pattern of the rest of the API, and were terrible clutter.
* Fixed a bug that caused `resultAt` to give wrong answers most of the time.
* Changed `resultAt` to an `IndexedLens` and moved it to `Control.Lens.IndexedLens`
* Changed `ignored` to an `IndexedTraversal` and moved it to `Control.Lens.IndexedTraversal`

3.2
---
* Made `elementOf` lazier and moved it from `Control.Lens.Traversal` to `Control.Lens.Plated`.
* Made `holesOf` and `partsOf` lazier to deal with infinite structures.
* Resolved issue #75. We now generate nicer core for most `Setter` and `Fold` operations, and some others.
* Made lenses for field access like `_1`, `_2`, etc. lazier.
* Added `Control.Lens.Loupe`, which provides a limited form of `Lens` that can be read from and written to and which can compose
  with other lenses, but can also be returned in a list or as a monadic result, but cannot be used directly for most combinators
  without cloning it first. It is easier to compose than a `ReifiedLens`, but slightly slower.
* Moved (`:=>`) and (`:->`) into `Control.Lens.Simple`, which is not exported by `Control.Lens` by default to reduce name conflicts with third party libraries.

3.1
---
* Simplified the type of `filtered`, so that it can be composed with other folds rather than be parameterized on one. Included the caveat that the new `filtered` is still not a legal `Traversal`, despite seeming to compose like one.
* Renamed `ifiltered` to `ifiltering`, and while it still must take an indexed lens-like as an argument, I included a similar caveat about the result not being a legal `IndexedLens` when given an `IndexedLens`. The function was renamed because its signature no longer lined up with the new `filtered` and the gerundive '-ing' suffix has come to indicate an operator that transformers another lens/traversal/etc. into a new one.
* Added `taking` and `dropping` to `Control.Lens.Traversal`.

3.0.6
-----
* Alpha-renamed all combinators to a new scheme. Instead of `Foo a b c d`, they now follow `Foo s t a b`. This means that you don't need to alpha rename everything in your head to work through the examples, simplifies exposition, and uses s and t for common state monad parameters. Thanks go to Shachaf Ben-Kiki for the grunt work of slogging through hundreds of definitions by hand and with regular expressions!
* Restored lenses to `Trustworthy` status so they can be used with Safe Haskell once more.

3.0.5
-----
* Fixed a bug in `rights1` and `lefts1` in `Control.Lens.Zipper` which would cause them to loop forever when given a 0 offset.

3.0.4
-----
* Added `?~`, `<?~`, `?=` and `<?=` to `Control.Lens.Setter` for setting the target(s) of a Lens to `Just` a value. They are particularly useful when combined with `at`.

3.0.3
-----
* Refined the behavior of `substType` in `Control.Lens.TH` to match the behavior of `typeVarsEx` when moving under binders.


3.0.2
-----
* Added `generateSignatures` option to `Control.Lens.TH` to allow the end user to disable the generation of type signatures for the
  template-haskell generated lenses. This lets the user supply hand-written haddocks and more restricted signatures.

3.0.1
-----
* Added `Control.Lens.Type.simple`.

3.0
---
* Added `Control.Lens.Zipper`.
* Added `<<~`, a version of `<~` that supports chaining assignment.
* Added `:->`, `:=>`, and `:<->` as type operator aliases for `Simple Lens`, `Simple Traversal`, and `Simple Iso`  respectively.

2.9
---
* Added `<<%~`, `<<.~`, `<<%=` and `<<.=` for accessing the old values targeted by a `Lens` (or a summary of those targeted by a `Traversal`)
* Renamed `|>` to `%`, as `%~` is the lensed version of `%`, and moved it to `Control.Lens.Getter` along with a version `^%` with tighter
  precedence that can be interleaved with `^.`
* Upgraded to `doctest` 0.9, which lets us factor out common `$setup` for our doctests
* Renamed `merged` to `choosing`. Added a simpler `chosen` operation to mirror `both`.
* Added `Control.Lens.Projection`
* Renamed `traverseException` to `exception` and `traverseDynamic` to `dynamic`, upgrading them to use `Projection`.
* `makeClassy` now places each generated `Lens` or `Traversal` inside the class it constructs when possible.
  This makes it possible for users to just export `HasFoo(..)`, rather than have to enumerate each lens in
  the export list. It can only do that if it creates the class. If the `createClass` flag is disabled, then
  it will default to the old behavior.
* Added `performs` to `Control.Lens.Action` to mirror `views` in `Control.Lens.Getter`.

2.8
---
* Restored compatibility with GHC 7.2. This required a major version bump due to making some MPTC-based default signatures conditional.

2.7.0.1
-------
* Added the missing `Control.Lens.Combinators` to exported-modules! Its absence was causing it not to be included on hackage.

2.7
---
* Generalized the signature of `Getting`, `Acting` and `IndexedGetting` to help out with the common user code scenario of needing to read
  and then write to change types.
* Documentation cleanup and additional examples.
* Renamed `au` to `ala`, introducing further incompatibility with the `newtype` package, but reducing confusion.
* Removed need for `Data.Map.Lens` and `Data.IntMap.Lens` by adding `TraverseMin` and `TraverseMax` to `Control.Lens.IndexedTraversal`.
* Flipped fixity of `~:` and `<~:`
* Added `++~`, `++=`, `<++~` and `<++=` to Data.List.Lens in response to popular demand.
* Added `|>`, `<$!>` and `<$!` to `Control.Lens.Combinators`, which exports combinators that are often useful in lens-based code, but that
  don't strictly involve lenses.
* Added an HUnit-based test suite by @orenbenkiki

2.6.1
-----
* Fixed bugs in `Traversal` code-generation.

2.6
---
* Added build option `-f-inlining` to facilitate building with the various TH 2.8 versions used by GHC 7.6 and HEAD.
* Added build option `-f-template-haskell` for testing without template haskell. (Users should be able to assume TH is enabled; use this only for testing!)
* Added support for generating a `Traversal` rather than a `Lens` when multiple fields map to the same name or some constructors are missing a field.
* Removed `_` from the lens names in `System.FilePath.Lens`.
* Added `iwhere`, `withIndices`, `withIndicesOf`, `indices` and `indicesOf` to ease work with indexed traversals
* Added `assign` as an alias for `(.=)` in `Control.Lens.Setter`.
* Added `~:`, `=:`, `<~:` and `<=:` to `Data.List.Lens`

2.5
---
* Added `Control.Lens.Plated`, a port of Neil Mitchell's `uniplate` that can be used on any `Traversal`.
* Added `Data.Data.Lens` with smart traversals that know how to avoid traversing parts of a structure that can't contain a given type.
* Added `Data.Typeable.Lens` with `_cast` and `_gcast` like `traverseData`
* Renamed `IndexedStore` to `Context` now that it is used in user-visible locations, and since I also use it as `uniplate`'s notion of a context.
* Renamed `Kleene` to `Bazaar` -- "a bazaar contains a bunch of stores."
* Added `Comonad` instances for `Context` and `Bazaar`, so we can use stores directly as the notion of an editable context in uniplate
* Compatibility with both sets of template haskell quirks for GHC 7.6.1-rc1 and the GHC 7.6.1 development head.
* Renamed `children` to `branches` in `Data.Tree.Lens`.
* Added `At` and `Contains` to `Control.Lens.IndexedLens`.
* Added `FunctorWithIndex`, `FoldableWithIndex`, and `TraversableWithIndex` under `Control.Lens.WithIndex`
* Added support for `unordered-containers`.

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
