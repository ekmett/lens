3.7.0.1
-------
* Corrected bounds for hashable.
* Fixed compatibility with Haskell Platform 2011.4.0.0 -- you may have to install with --constraint="transformers = 0.2.2.0" to avoid getting new mtl and transformer versions installed.

[3.7](https://github.com/ekmett/lens/issues?milestone=11&page=1&state=closed)
-----
* Renamed `Projection` to `Prism`.
* Implemented a complete redesign of the way `Iso` and `Prism` are handled internally. Any `Iso` can now be used as a `Prism`.
* The `isos` combinator is no longer required. `iso` can now be used to construct an `Iso`.
* Changes to the signature of `from` and `under` were necessitated by the new design.
* Added `Control.Lens.Wrapped` providing a canonical isomorphism for newtypes.
* Repurposed `ala` to be closer to the original design in `newtype`, but added `au` and `alaf`.
* Added `_magnitude`, `_phase` and `_conjugate` to `Data.Complex.Lens`. Renamed other lenses for consistency: `_realPart`, `_imagPart`, `_polar`.
* Promoted `_left` and `_right` to prisms and moved them to `Control.Lens.Prism`.
* Generalized `view` and `views` to subsume the old functionality of `peruse` and `peruses`.
* Generalized `review` and `reviews` to both return a `MonadReader` and to work on a `Projection`.
* Added `view'`/`views'` and `use'`/`uses'` for `Simple` access to the environment/state.
* Added `set'`, a `Simple` version of `set`.
* Added `reuse` : `use` :: `review` : `view` and `reuses` : `uses` :: `reviews` : `views` for working a `Projection` from the current `MonadState`.
* Removed many isomorphisms for various newtypes. `_const`, `identity`, `_sum`, etc. Use `wrapping Const`, `wrapping Identity`, etc.
* Removed `Data.Monoid.Lens` now that its newtypes are instances of `Wrapped`, exporting the (`<>=`)-variants from `Control.Lens.*`.
* Renamed `via` to `cloneIso` for consistency.
* Moved `Indexed(..)` to `Control.Lens.Classes`.
* Renamed `index` to `indexed` to reduce conflicts with third-party libraries.
* Added `curried` and `uncurried` to `Control.Lens.Iso`.
* Added `Strict(strict)` for ad hoc overloading of conversions between strict and lazy variants of `ByteString` and `Text`.
* Bug fixes for `tugTo` and `jerkTo`.
* These no longer traverse in the wrong direction: `scanl1Of`, `scanr1Of`, `mapAccumLOf`, and `mapAccumROf`.
* Added `anon` to `Control.Lens.Iso`.
* Generalized the types of the `Control.Lens.Zipper` combinators to work with other MonadPlus instances.
* Added `withins` to `Control.Lens.Zipper` now that they can work better with [].
* Added `singular` and `unsafeSingular` to `Control.Lens.Traversal` to assert a `Traversal` is a `Lens`, a `Fold` is a `Getter` or a `MonadicFold` is an `Action`.
* Generalized `sequenceAOf_`'s type to match `sequenceA_`.
* Renamed `up`/`down`/`left`/`right` to `upward`/`downward`/`leftward`/`rightward` to reduce conflicts -- in particular with `Control.Arrow`.
* Readded `leftmost` and `rightmost` due to the verbosity of `farthest leftward`/`farthest rightward`.
* Added `preview`/`previews`/`firstOf` and deprecated `headOf`.
* Added `iview`/`iviews`/`iuse`/`iuses` to `Control.Lens.IndexedGetter`.

3.6.0.4 [maintenance release]
-------
* Added support for `test-framework` 0.8

3.6.0.3 [maintenance release]
-------
* Added support for `test-framework` 0.7

3.6.0.2 [maintenance release]
-------
* Added more explicit dependencies to the doctest suite.
* Disabled the 'expected failure' quickcheck tests that occasionally would fail with internal QuickCheck errors.

3.6.0.1 [maintenance release]
-------
* Added explicit dependency on containers and unordered-containers to the doctest suite

[3.6](https://github.com/ekmett/lens/issues?milestone=9&state=closed)
---
* Added `upon` (along with variants of it) to `Data.Data.Lens`, which can be used to generate a `Traversal` from a field accessor or any function that returns, unmodified,
  a single field that would be visited by `template`.
* Added some missing `examples/` files to the distribution.
* Renamed `Data.Bits.Lens.traverseBits` to `bits`.
* Removed `(^!?)`, which was an alias for `(^?!)`.
* Removed the need for `Trustworthy` by changing the implementation of `coerce` for `BazaarT`.
* Moved BazaarT to `Control.Lens.Internal`.
* Added `(<&>)` to `Control.Lens.Combinators`.
* `element` and `elementOf` are now indexed traversals rather than lenses and have moved to `Control.Lens.IndexedTraversal`. This both fixes their former partiality and lets you use chain indexed combinators with them.
* Added `elements` and `elementsOf` as indexed traversals for ordinal indexing into regular traversals that generalize `element` and `elementOf`.
* Renamed `Data.Complex.Lens.traverseComplex` to `complex`.
* Changed `Data.Complex.Lens.polarize` to a `Simple Iso`, due to the `RealFloat` constraint causing inference problems.
* Renamed `traverseLeft` and `traverseRight` to `_left` and `_right` respectively.
* Renamed `traverseSlice`, `traverseFrom`, and `traverseTo` in `Data.Sequence.Lens` to `sliced`, `slicedFrom`, and `slicedTo` respectively.
* Renamed `traverseAt` to `_at` in `Control.Lens.IndexedTraversal`.
* Renamed `traverseArray` to `_array` in `Data.Array.Lens`.
* Renamed and made the combinators in `Control.Lens.Zipper` more compositional to reduce third-party naming conflicts down to just `left` and `right`.
* Renamed `&=` and `|=` to `.&.=` and `.|.=` for consistency, mutatis mutandis their related operations.
* Added a `Plated` instances for `Language.Haskell.TH` types.
* Renamed `atIndex` and `atIndices` in `Data.Vector.Lens` and `Data.Vector.Generic.Lens` to `ordinal` and `ordinals` to match `Data.Sequence.Lens`

3.5.1
-----
* Improved SafeHaskell inference.

[3.5](https://github.com/ekmett/lens/issues?milestone=8&state=closed)
---
* Fixed a potential SafeHaskell issue where a user could use `undefined` to derive `unsafeCoerce`. You now have to import an explicitly
  Unsafe module and create an instance of `Trustworthy` for your type to cause this behavior, so if you do, its on your head, not mine. :)
* Renamed `EvilBazaar` to `BazaarT`.
* Moved a lot of internals around. Most notably, `Gettable`, `Settable` and `Effective` have moved to `Control.Lens.Classes`.
* Exposed `partsOf'` and `unsafePartsOf'` in `Control.Lens.Traversal` to reduce reliance on `BazaarT` in `Control.Lens.Zipper`

[3.4](https://github.com/ekmett/lens/issues?milestone=7&state=closed)
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

[3.0](https://github.com/ekmett/lens/issues?milestone=6&state=closed)
---
* Added `Control.Lens.Zipper`.
* Added `<<~`, a version of `<~` that supports chaining assignment.
* Added `:->`, `:=>`, and `:<->` as type operator aliases for `Simple Lens`, `Simple Traversal`, and `Simple Iso`  respectively.

[2.9](https://github.com/ekmett/lens/issues?milestone=5&state=closed)
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

[2.8](https://github.com/ekmett/lens/issues?milestone=4&state=closed)
---
* Restored compatibility with GHC 7.2. This required a major version bump due to making some MPTC-based default signatures conditional.

2.7.0.1
-------
* Added the missing `Control.Lens.Combinators` to exported-modules! Its absence was causing it not to be included on hackage.

[2.7](https://github.com/ekmett/lens/issues?milestone=3&state=closed)
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

[2.6](https://github.com/ekmett/lens/issues?milestone=2&state=closed)
---
* Added build option `-f-inlining` to facilitate building with the various TH 2.8 versions used by GHC 7.6 and HEAD.
* Added build option `-f-template-haskell` for testing without template haskell. (Users should be able to assume TH is enabled; use this only for testing!)
* Added support for generating a `Traversal` rather than a `Lens` when multiple fields map to the same name or some constructors are missing a field.
* Removed `_` from the lens names in `System.FilePath.Lens`.
* Added `iwhere`, `withIndices`, `withIndicesOf`, `indices` and `indicesOf` to ease work with indexed traversals
* Added `assign` as an alias for `(.=)` in `Control.Lens.Setter`.
* Added `~:`, `=:`, `<~:` and `<=:` to `Data.List.Lens`

[2.5](https://github.com/ekmett/lens/issues?milestone=1&state=closed)
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
