5.1 [????.??.??]
----------------
* Removed `Wrapped` and `Rewrapped` instances for `Data.Semigroup.Option`,
  which was removed in `base-4.16`.
* Allow building with GHC 9.2.
* Drop support for GHC 7.10 and older.
* Define `_CharTyLit` in `Language.Haskell.TH.Lens` when building with
  `template-haskell-2.18` or later.

5.0.1 [2021.02.24]
------------------
* Fix a bug in which `makeLenses` could produce ill kinded optics for
  poly-kinded datatypes in certain situations.

5 [2021.02.17]
--------------
* Support building with GHC 9.0.
* Remove the `Swapped` type class in favor of `Swap` from the `assoc` package.
* Remove the `Strict` type class in favor of `Strict` from the `strict` package.

  The `swapped`, `strict` and `lazy` isomorphisms are now defined using "new" type classes.

  Users which define own instances of old type classes are advised to
  define instances of the new ones.

  ```haskell
  import qualified Data.Bifunctor.Swap as Swap
  import qualified Control.Lens        as Lens

  instance Swap.Swap MyType where
    swap = ...

  #if !MIN_VERSION_lens(4,20,0)
  instance Lens.Swapped MyType where
    swapped = iso Swap.swap Swap.swap
  #endif
  ```
* The `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex` type classes
  have been migrated to a new package,
  [`indexed-traversable`](https://hackage.haskell.org/package/indexed-traversable).

  The `imapped`, `ifolded` and `itraversed` methods are now top-level functions.
  If you are not defining these methods in your instances,
  you don't need to change your definitions.

  Beware: the `optics-core` package (versions <0.4) defines similar classes,
  and will also migrate to use `indexed-traversable` classes. Therefore, you
  might get duplicate instance errors if your package defines both.

  If you define your own `FunctorWithIndex` etc. instances,
  we recommend that you depend directly on the `indexed-traversable` package.
  If you want to continue support `lens-4` users, you may write

  ```haskell
  -- from indexed-traversable
  import Data.Functor.WithIndex

  -- from lens
  import qualified Control.Lens as L

  -- your (indexed) container
  data MySeq a = ...

  -- indexed-traversable instance
  instance FunctorWithIndex     Int MySeq where imap = ...
  instance FoldableWithIndex    Int MySeq where ifoldMap = ...
  instance TraversableWithIndex Int MySeq where itraverse = ...

  -- lens <5 instance, note the !
  #if !MIN_VERSION_lens(5,0,0)
  instance L.FunctorWithIndex     Int MySeq where imap = imap
  instance L.FoldableWithIndex    Int MySeq where ifoldMap = ifoldMap
  instance L.TraversableWithIndex Int MySeq where itraverse = itraverse
  #endif
  ```

  In other words, always provide `indexed-traversable` instances.
  If your package depends on `lens` and allows `lens-4`,
  you should additionally provide instances for `lens-4` type classes
  that can reuse the `indexed-traversable` instances.

* Make the functions in `Control.Lens.TH` work more robustly with poly-kinded
  data types. This can cause a breaking change under certain situations:
  * TH-generated optics for poly-kinded data types are now much more likely to
    mention kind variables in their definitions, which will require enabling
    the `PolyKinds` extension at use sites in order to typecheck.
  * Because TH-generated optics now quantify more kind variables than they did
    previously, this can affect the order of visible type applications.
* Generalize the types of `generic` and `generic1` to allow type-changing
  updates. If you wish to use the old, more restricted types of these
  functions, use `simple . generic` or `simple . generic1` instead.
* Add `Control.Lens.Profunctor` with conversion functions to and from
  profunctor optic representation.
* Add `Control.Lens.Review.reviewing`, which is like `review` but with a more
  polymorphic type.
* Mark `Control.Lens.Equality` as Trustworthy.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec) to run the doctests.
* Use `alterF` in `At (HashMap k)` instance implementation.
* Use `alterF` in `At` and `Contains` instances for `Set`, `IntSet`, and
  `HashSet`.
* Avoid re-inserting keys already present in `ix` for `Set`, `IntSet`,
  and `HashSet`. For `Set` and `HashSet`, this changes the semantics
  slightly; if the user-supplied key is `==` to one already present in
  the set, then the latter will not be replaced in the result.
* Consume `()` values lazily in `Control.Lens.At`.

4.19.2 [2020.04.15]
-------------------
* Remove the test suite's dependency on `test-framework-th`.

4.19.1 [2020.02.13]
-------------------
* Fix a bug introduced in 4.19 where using `_TupE` to `preview` a value would
  always fail.

4.19 [2020.02.03]
-----------------
* Support building with GHC 8.10.
* The types of `_TupE` and `_UnboxedTupE` are now `Prism' Exp [Maybe Exp]`
  when built against `template-haskell-2.16` or later to reflect the new
  types of `TupE` and `UnboxedTupE`.
* Add `_ForallVisT` and `_BytesPrimL` prisms when building against
  `template-haskell-2.16` or later.
* Make `<>~` and `<>=` and their `<op` and `<<op` state variants require only
  `Semigroup`, not `Monoid`.
* Add `{Functor,Foldable,Traversable}WithIndex` instances for
  `Control.Applicative.Const` and `Data.Functor.Constant.Constant`.

4.18.1 [2019.09.13]
-------------------
* Remove the use of `cpp-options: -traditional`. This should be unnecessary
  on all versions of GHC that `lens` supports, as modern GHCs already use
  `-traditional` internally during preprocessing. More critically, the use
  of `cpp-options: -traditional` breaks profiling builds on GHC 8.8
  (see https://gitlab.haskell.org/ghc/ghc/issues/17185).

4.18 [2019.09.06]
-----------------
* Support building with GHC 8.8.
* Add `xplat` and `xplatf`.
* Flip `auf` to take the `Iso` in the same direction as `au`.
  Use the new `xplatf` or just call `coerce` for the old form.
* Weaken `holeInOne`'s `Category p` constraint to `Comonad (Corep p)`.
* Generalize the type of `GHC.Generics.Lens.generic1` from
  `Iso' (f a) (Rep1 f a)` to `Iso (f a) (f b) (Rep1 f a) (Rep1 f b)`.
* `makeClassyPrisms` now supports generating prisms for data types that share
  a name with one of its constructors. In such a scenario, the name of the
  classy prism for the data type will be prefixed with an extra `_` (for
  prefix names) or `.` (for infix names) to disambiguate it from the prism
  for the constructor.
* Several type classes in `Control.Exception.Lens` now have additional
  prisms corresponding to the data type that they focus on, in accordance
  with the new convention established in the bullet point above. For example,
  `AsNonTermination` now has an additional
  `__NonTermination :: Prism' t NonTermination` method, and the existing
  `_NonTermination :: Prism' t ()` method now has a default implementation in
  terms of `__NonTermination`.

  As a consequence of this change, you may need to update instances of these
  classes to implement the new prisms.
* Add additional bifunctor instances for `Swapped`.
* New lenses `head1` and `last1`, to access the first/last elements of
  a `Traversable1` container.
* Add `filteredBy`.
* Add `adjoin` to `Control.Lens.Unsound`.
* Add `Each (Either a a) (Either b b) a b` instance.
* Make `magnify` offer its getter argument the `Contravariant` and `Functor`
  instances it will require. This allows `magnify` to be used without
  knowing the concrete monad being magnified.
* Add `equality`, `equality'`, `withEquality`, `underEquality`, `overEquality`,
  `fromLeibniz`, `fromLeibniz'` and `cloneEquality` to `Control.Lens.Equality`.
* Add `withLens` to `Control.Lens.Lens`.
* Make `substEq` and `simply` in `Control.Lens.Equality`
  and `withIso` in `Control.Lens.Iso` levity polymorphic.

4.17.1 [2019.04.26]
-------------------
* Support `th-abstraction-0.3.0.0` or later.
* Only incur `semigroups` and `void` dependencies on old GHCs.
* Add `holes1Of`
* Add `locally` https://github.com/ekmett/lens/pull/829
* Add `ilocally` https://github.com/ekmett/lens/pull/836
* Add third `Prism` law.
* Add `gplate1`
* Add `Wrapped`/`Rewrapped` instances for `Data.Monoid.Ap`.

4.17 [2018.07.03]
-----------------
* Allow building with GHC 8.6.
* Make the instances for `Product` and `(:*:)` in `Control.Lens.Tuple`
  poly-kinded.
* Make the definitions in `GHC.Generics.Lens` poly-kinded.
* Make `(%%@~)`, `(%%@=)`, `(<%@=)`, and `(<<%@=)` consume an
  `Over (Indexed i)` instead of an `IndexedLensLike i` to improve type
  inference.
* Add an `AsEmpty` instance for `ZipList`.

4.16.1 [2018.03.23]
-------------------
* Re-export `(<&>)` from `Data.Functor` on `base-4.11` and later.
* Added `Cons` and `Snoc` instances for `Control.Applicative.ZipList`
* Fix a bug in which `makeFields` would generate equality constraints for
  field types involving data families, which are unnecessary.
* Improve the performance of `holesOf`.

4.16 [2018.01.28]
-----------------
* The `Semigroup` instances for `Traversed` and `Sequenced` are now more
  constrained (going from `Apply` to `Applicative` and `Monad`, respectively).
  In GHC 8.4, `Semigroup` is a superclass of `Monoid`, therefore we'd need to
  have `Apply` constraint in the `Monoid` instances. We opted to weaken our
  ability to use `Apply` than to lose compatibility with third-party packages
  that don't supply instances for `Apply`.

  In practice this changes the (specialised) type signature of `traverseOf_`
  ```diff+
  - traverseOf_ :: Apply f       => Fold1 s a -> (a -> f r) -> s -> f ()
  + traverseOf_ :: Applicative f => Fold1 s a -> (a -> f r) -> s -> f ()
  ```
  and similarly for `forOf_` and `sequenceOf_`.

  As part of this change, new combinators `traverse1Of_`, `for1Of_` and
  `sequence1Of_` were added for `Apply`-only effects.

  Similar instance context changes were made for `Folding` and `Effect`,
  but these changes aren't publicly visible.

* Add `Control.Lens.Unsound`, which exports unsound functionality for forming
  products of lenses and sums of prisms.

* Add `Numeric.Natural.Lens`, which export convenient isomorphisms for
  natural numbers.

* Add `Strict` instances for strict and lazy `ST`.

* Adapt `Language.Haskell.TH.Lens` for `template-haskell-2.13` (bundled
  with GHC 8.4).

* Add `Semigroup` and `Monoid` instances for `Indexing`.

4.15.4
----
* `makeFields` and `declareFields` are now smarter with respect to type
  families. Because GHC does not allow mentioning type families in instance
  heads, the Template Haskell machinery works around this restriction by
  instead generating instances of the form:

  ```haskell
  type family Fam a
  data Rec a = Rec { _recFam :: Fam a }
  makeFields ''Rec

  ===>

  instance (b ~ Fam a) => HasFam (Rec a) b where ...
  ```

  This requires enabling the `UndecidableInstances` extension, so this trick is
  only employed when a field's type contains a type family application.
* `declareFields` now avoids creating duplicate field classes that are shared
  among multiple datatypes within the same invocation.
* The Template Haskell machinery will no longer generate optics for fields
  whose types mention existentially quantified type variables.
* Add `HasCallStack` constraints to partial operations
* Reexport `(.@~)` and `(.@=)` from `Control.Lens.Operators`
* Support `doctest-0.13`

4.15.3
----
* Generalized types of `transformMOf`, `transformOf`, `transformMOnOf`,
  `transformOnOf`, `rewriteMOf`, `rewriteOf`, `rewriteMOnOf` and `rewriteOnOf`.
* Depend on `th-abstraction` package for normalizing differences across
  `template-haskell` versions

4.15.2
----
* Build with GHC 8.2
* Expand tuple accessors to support up to 19-tuples
* Add more `Rewrapped` and `Wrapped` instances for data types from the `base`,
  `bifunctors`, `exceptions`, `free`, `profunctors`, and `semigroupoids`
  libraries
* Add a `Generic` default implementation for `Wrapped`
* Add `Wrapped` instances for data types introduced in `Foreign.C.Types` and
  `System.Posix.Types` in `base-4.10.0.0`
* Add prisms for recently introduced data types in `Control.Exception`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Add `makeFieldsNoPrefix`, a variant of `makeFields` which gives the desired
  behavior in the presence of `DuplicateRecordFields`. Also add
  `classUnderscoreNoPrefixFields` and `classUnderscoreNoPrefixNamer`, the
  corresponding `LensRules` and `FieldNamer`, respectively.
* Add `toNonEmptyOf`, `first1Of`, `last1Of`, `minimum1Of`, and `maximum1Of`
  to `Control.Lens.Fold`
* Add `both1` to `Control.Lens.Traversal`
* Generalize the type of `levels` and `ilevels` in `Control.Lens.Level` to work
  on `Fold`s
* Generalize the type of `getting` in `Control.Lens.Getter` to work with any
  `Optical`
* Add `throwing_` to `Control.Monad.Error.Lens` and `Control.Exception.Lens`
* Fix the meta-data in the .cabal file to properly indicate that this project
  has a BSD2 license

4.15.1
----
* Restore the `generic` and `generic1` functions in `GHC.Generics.Lens`

4.15
----
* Remove `Generics.Deriving.Lens` module.
* Incorporate `URec`, which was introduced in `GHC.Generics` in `base-4.9`. For compatibility with older versions of `base`, `lens` now conditionally depends on `generic-deriving`
* Add `Rewrapped` instance for `ExceptT`
* Add `FunctorWithIndex`, `FoldableWithIndex`, and `TraversableWithIndex` instances for `Sum`, `Proxy`, `Tagged` and data types in `GHC.Generics`
* Remove unneeded context from `*WithIndex HashMap` instances
* Add `Data.Map.Lens.toMapOf`
* Add moral `Functor` constraint for `to` `ito` `ilike` `ilike` to allow the
  "indented" type signature using Getter with redundant warnings turned on.

4.14
----
* Remove `Cons` and `Snoc` instances for `NonEmpty`.

4.13.2.1
------
* Fixed `itraverse_` and `imapM_` returning bottom

4.13.2
------
* Restore default signature for `Control.Lens.At.at`
* Improve operations for `Data.Sequence.Seq`
* Fix `declarePrisms` behavior on GHC 8 using GADT record syntax

4.13.1
------
* Modified to enable the `doctests` to build with `stack`.
* Removed `.ghci`.
* Added `lookupOf`
* Support GHC 8
* Support `transformers` 0.5
* Support `kan-extensions` 5
* Support `comonad` 5
* Better support for `Closed` from `profunctors`.

4.13
----
* Pattern synonyms
* Moved `foldMapBy` and `foldBy` into `reflection` 2.1
* Added `traverseByOf`, `sequenceByOf`.
* Reexported `traverseBy` and `sequenceBy` from `reflection` 2.1.
* Modified the signatures of `alaf` and `auf` to work with a `Functor` rather than a `Profunctor` and rather drastically generalized them.
* Removed `Control.Lens.Internal.Getter.coerce` in favor of the upstream `phantom` combinator in `contravariant` 1.3+
* Renamed `coerced` to `phantasm` to get it out of the way.
* Added `Wrapped` instance for `Down`

4.12.3
------
* Move `Review` and `AReview` to `Control.Lens.Type` fixing a bug in `makePrisms`
* Expose `HasTypes` class in `Language.Haskell.TH.Lens`
* Make types of `foldByOf` and `foldMapByOf` more specific to hide implementation details
* Add Prisms to `Language.Haskell.TH` for new constructors in `template-haskell-2.10`
* Generalize type of `_FunDep` to an `Iso`

4.12.2
------
* Incorporated a bug fix for `foldByOf` and `foldMapByOf` to actually let them work on folds.
* Added a `Plated` instance for `CofreeT`

4.12.1
------
* The `Simple` type alias is now poly-kinded. This lets you use `Simple Field1 s a` and the like in constraints.
* Added `HasTypes` to `Language.Haskell.TH.Lens`.
* Support for `vector-0.11.0` which changes `Stream` to `Bundle`

4.12
----
* `reflection 2` support.

4.11.2
------
* Give `cosmosOn` a more general type.

4.11.1
------
* Added `cosmos`, `cosmosOf`, `cosmosOn`, `cosmosOnOf` to `Control.Lens.Plated`.
* Added `icontains`, `iat`, `iix`.
* Made various documentation improvements.
* Added a `test-templates` flag.

4.11
----
* Proper `profunctors` 5.1 support. This extended the superclass constraints for `Conjoined`, so it resulted in a major version bump.

4.10
----
* Added `elemIndexOf`, `elemIndicesOf`, `findIndexOf`, and `findIndicesOf`.
* Fixed `Ixed` instance for `Tree`. It no longer drops nodes prior to the traversed node.
* `bifunctors` 5, `profunctors` 5 and `semigroupoids` 5 support.

4.9.1
-----
* Added `_Wrapped` support for `NonEmpty`.
* Added `_Wrapped` support for `Alt`.
* Fixed `Rewrapped` instance for `Last`.

4.9
-------
* `filepath` 1.4 support
* Removed `Control.Monad.Primitive.Lens` and shed the `primitive` dependency.
* Add missing `_WithIndex` instances from `keys` package
* Much more code is inferred `Safe` rather than `Trustworthy`.
* Documented the difference between `unsafeSingular` and `singular`.
* `folding` now produces an actual `Fold`.
* Cleaned up builds for GHC 7.10 to get rid of redundant import warnings.

4.8
---
* When built with `profunctors` 4.4 on GHC 7.8+ we no longer need to use `unsafeCoerce` at all!
  This drastically reduces the level of trust involved in the way we have optimized `lens`.
* Added `fusing`. This optimizes long `Lens` chains, by enfocing a form of `fmap` fusion based on the Yoneda lemma. This is particularly effective at making faster lenses the definition is recursive or complex enough that it cannot be inlined.
* Added `confusing`. This optimizes long `Traversal` chains. As with `fusing` it is best used when the definition for the `Traversal` chain in question is recursive or complex enough that it cannot be inlined, but the implementation is much more confusing.
* Remove deprecated stuff: `Control.Lens.Loupe`, `headOf`, `makeFieldsWith`,
  `strippingPrefix`, `strippingSuffix`
* Added `Cons` and `Snoc` instances for `NonEmpty`
* Removed `Data.List.Split.Lens` module
* Reimplemented `bytestring` traversals to avoid internal modules
* Added `gplate`, an implementation of `plate` for any type implementing `Generic`
* Strictness revisited
  * Add `generateLazyPatterns` configuration flag to `makeLenses` rules.
  * Make the default `makeLenses` behavior to generate STRICT optics
  * Add strict variants of `_1` .. `_9` named `_1'` .. `_9'`
* Generalized some combinators in `Data.Vector.Generic.Lens` and added `converted`

4.7
---
* Migrated `Control.Lens.Action` to `lens-action`.
* Added `Data.Vector.Generic.Lens.vectorIx` function for indexing vectors with only `Vector` constraint.
* Added `Field1` and `Field2` instances for `Data.Functor.Product.Product`.
* Removed the "typeclass synonym" `Gettable`.
* Added new flag to `makeLenses`, `generateUpdateableOptics`, which allows
  the generation of only `Getter`s and `Fold`s. This feature is intended
  to be used when the constructors are hidden behind validating, "smart"
  constructors.
* Fixed Template Haskell name generation when using GHC 7.10
* Fixed Template Haskell generation of classes methods where field types used
  existential quantification

4.6.0.1 [maintenance release]
-------
* Compatibility with `base` 4.8 [Edit: this turned out to not work for the final release of GHC 7.10]

4.6
---
* Reduced `Review` to two arguments, like `Getter`.
* Added `abbreviatedFields` to permit `makeFieldsWith` to be invoked with an argument that lets it act like it did pre-4.5 and accept arbitrary common prefixes.

4.5
---
* Provide access to the typename in `lensRules` naming function.
* `makeFields` camelcasing rules now properly support types with camelcasing. `MyType` with field `myTypeFieldA` generates `fieldA` now. Previously the prefix ignore capitalization and the field would need to be named `mytypeFieldA`.
* `makeClassy` works on types even when none of the fields would generate optics.
* Added `Monad`, `MonadReader`, `MonadPlus` and `Bind` instances for `ReifiedMonadicFold`
* Added missing fixity declarations on many operators.
* Migrated `Codec.Compression.Zlib.Lens` to `zlib-lens` package.

4.4.0.2
---
* `text` 1.2.0.0 support
* Remove the use of the TemplateHaskell extension from the library to enable lens to be used on stage1 cross-compilers

4.4.0.1
----
* Restore previous default of `makeFields` using the camel case field namer.

4.4
----
* Internals of Template Haskell code generation rewritten. makeLenses,
  makeClassy, and makeFields have been unified into the same generator.
* TH generated single constructor Lens use irrefutable pattern matching to
  enable construction starting with undefined.
* TH generated traverals unify their field arguments (type synonyms not
  currently expanded) enabling exotic traversals to be generated.
* Added instances for `Text` to `Data.Aeson.Lens`
* Reimplemented `makePrisms`, adding support for `makeClassyPrisms`, infix constructrs generate periods (.) prefixed prisms.
* Added `Choice` to `Review` so that `Prism` is a proper subtype of `Review`
* Migrated `Data.Aeson.Lens` to `lens-aeson` package.
* Fixed `GHC.Generics.Lens.tinplate` behavior on single-field data types and empty data types.

4.3.3
-----
* `semigroupoids` 4.2 support

4.3.2
-----
* `contravariant` 1.0 support

4.3.1
-----
* Added `bytewise` to `Data.Bits`

4.3
---
* Switched the "direction" of the `Iso` argument to `au` to match the order generated by `makePrisms` and `makeLenses`.
* Removed `makeIsos` in favor of `makePrisms` and `makeLenses`. Each of these functions will construct `Iso`s when appropriate.
* Removed `declareIsos` in favor of `declarePrisms` and `declareLenses`. Each of these functions will construct `Iso`s when appropriate.
* Added `matching` for type-changing matches with `Prism`s.
* Added `withPrism` for recovering the functions passed to `prism`.
* Added `negated`, the isomorphism for the `negate` function.

4.2
---
* Added `_Text` isomorphisms to make the proper use with `(#)` more obvious and fit newer convention.
* Added `Wrapped` instances for `Vector` types
* Resolved issue #439.  The various `Prism`s for string-like types in `Data.Aeson.Lens` are now law-abiding `Prism`s "up to quotient."
* Added `selfIndex`.
* Support `attoparsec` 0.12.

4.1.2
-----
* When used with `exceptions` 0.4, `throwingM` will permit use with a mere `MonadThrow`.

4.1.1
----
* Generalized the types of `mapping`, `bimapping`, `contramapping`, `dimapping`, `lmapping`, `rmapping` to support changing the `Functor`, `Bifunctor`, `Contravariant`, and `Profunctor` respectively.
* Compatibility with `free` 4.6

4.1
---
* Added `Plated` instances for various free monad variants.
* Compatibility with GHC HEAD (7.9+)

4.0.7
-----
* Removed dependency on `constraints`. It was used in a pre-release version of 4.0 but never made it into 4.0, but the dependency had remained around complicating builds for GHC 7.4.

4.0.6
-----
* `makeLenses` attempt to make the accessors it can under existential quantification.
* Added `(&~)`.
* _Experimental_ support for parallel builds on GHC 7.8 with `cabal install lens -fj`. Due to at last one known issue with GHC, it isn't recommended to use this option when rebuilding lens, as a race condition on at least one platform has been seen in the wild.
* Added `RoleAnnotations` for GHC 7.8.1. These rule out a few user-accessible bottoms that could be caused by creative abuse of the new `Coercible` machinery. However, there was no `unsafeCoerce` exposed.
* Removed some impossible cases that required unwritable instances from the example doctypes.

4.0.5
-----
* Added `bimapping` to `Control.Lens.Iso`
* Restored correct behavior of `makePrism` on types with a single constructor.
* `makeLenses` now generates `Getter`s and `Fold`s on universally quantified fields.

4.0.4
-----
* Made `declareFields` work again.

4.0.3
-----
* Fixed random segfaulting when using `foldMapBy`.

4.0.2
-----
* Properly bundled the modules needed for the properties test suite into the tarball for hackage.

4.0.1
-----
* Typo fixes
* Exporting `Rewrapping` from `Control.Lens.Wrapped`.
* Removed the dependency on `cpphs`.

4.0
----
* Added `nearly` to `Control.Lens.Prism`.
* Added `Control.Lens.Empty`, exporting `_Empty`.
* We now require `DefaultSignatures`.
* Added `failing` and `ifailing` to `Control.Lens.Traversal`.
* Changed the signature of `Data.List.Split.Lens.condensing` due to the addition of `DropBlankFields` to `Data.List.Split.CondensePolicy` in `split`.
* Simplified `Each`, `Ixed`, and `Contains`. They are no longer indexed. The previous design was actively getting in the way of user-defined instances.
* Replaced more of our home-grown types with standard ones. They had previously been defined to help make more intelligible error messages, but when we switched to using `(Contravariant f, Functor f)` instead of `(Gettable f)`, these ceased to really help. Now you can define even more `lens`-compatible types (e.g. `Getter` and `Fold`) without depending on `lens`.
  * Replaced the use of `Accessor` with `Const`.
  * Replaced the use of `Mutator` with `Identity`.
  * Replaced the use of `Reviewed` with `Tagged`.
* Removed the deprecated `Control.Lens.Simple` module.
* Repurposed `Control.Lens.Combinators` to re-export `Control.Lens` sans any operators; previous residents rehomed to `Control.Lens.Lens`.
* Added `Control.Lens.Operators` to export just the operators. Varying your import styles between these supports many qualified usage scenarios.
* Simplified `Cons` and `Snoc`. Now they must be a `Prism`.
* Simplified `Contains`. This necessitated losing many instancs of `Contains`, but makes it much easier and more consistent to use and instantiate.
* Simplified the various `AsFoo` types in `Control.Exception.Lens`
* Simplified the types in `System.IO.Error.Lens`.
* Merged `lens-aeson` into `lens`.
* We're exiling `Control.Lens.Zipper` to a separate package. This will let the design for it iterate faster and let us explore the trade-offs between the 3.8 style and the 3.9 style of zippers.
* Generalized `alongside`, `inside`, `both`.
* Switched to a new `Typeable` version of `reflection` for the harder combinators in `Control.Exception.Lens`. This enables us to comply with GHC 7.7's ban on hand-written `Typeable` instances.
* Added a `_Show` `Prism`.
* Added `Control.Lens.Extras` for the combinator names we don't have the gall to claim outright, but which are consistent with the rest.
* Renamed the constructors for `ReifiedLens`, etc. to just be the name of their base type.
* Added many many missing instances for `ReifiedFold` and `ReifiedGetter`. This permits things like `runFold ((,) <$> Fold (traverse._1) <*> Fold (traverse._2))` to be a `Fold`
  and `ReifiedFold` can be used as a `Monad`, `Profunctor`, etc.
* Many performance optimizations.
* Switched to `exceptions` from `MonadCatchIO-transformers`
* Added types for working with `RelevantFold` and `RelevantTraversal`. These are a `Fold` or `Traversal` that always has at least one target. Since `Apply` isn't a superclass of `Applicative`, you occasionally need to convert between them, but it lets you more readily work with less unsafety.
* Changed `unwrapping` and `wrapping` to have the same constructor-oriented order as a `Prism` and renamed them t `_Wrapping` and `_Unwrapping` respectively.
* Drastically changed the way `_Wrapping` and `_Unwrapping` are built to get much better inference.
* There are about 15,000 lines of patches over the last year, so I'm sure we missed a few big changes.

3.10.1 [maintenance release]
------
* Compatibility with `base` 4.7

3.10.0.1 [maintenance release]
--------
* Compatibility with `text` 1.0

3.10
----
* Switched to `bifunctors`, `comonad`, `profunctors`, and `semigroupoids` 4.0.

3.9.2
-----
* Generalized signatures for `throwing` and `throwingM`.

3.9.1
-----
* 'condensingPolicy' was updated to work with 'split' 0.2.2

3.9.0.3
-------
* Bumped dependency on `generic-deriving` again.

3.9.0.2
-------
* Bumped dependency on `generic-deriving` to enable building on GHC HEAD.

3.9.0.1
-------
* Updated the field guide image to link to imgur. Sadly the overview haddock and the haddocks are not generated in the same directory, so the haddock hook for copying the image only works locally.

3.9
-----
* Changed `Getting` to take 3 arguments instead of 5. If you need the old behavior for portability you can use
  `Overloaded (Accessor r) s t a b` instead of `Getting r s t a b` and it'll work consistently back through the last few releases.
* Added `involuted` to `Control.Lens.Iso`.
* Factored out a common `reversed` definition from all the various forms of it around the library and placed it in `Control.Lens.Iso`.
* Added `binary`, `octal`, `decimal` and `hex` to `Numeric.Lens`.
* Added `sans` to `Control.Lens.At`.
* Improved interoperability:
  * Reimplemented `Gettable` as an alias for `Contravariant` and `Functor` together to derive `Getter` and `Fold`. This means you can now
    implement a `Getter` or `Fold` with only a Haskell 98 dependency (`contravariant`).
  * Removed `Reviewable`. We now use `Bifunctor` and `Profunctor` together to derive `Review`. This means you can now implement a `Review`
    with Haskell 98 dependencies (`profunctors` and `bifunctors`).
  * These changes enables more types to be defined without incurring a dependency on the `lens` package.

3.8.7.0-3.8.7.3 [maintenance releases]
-----
* Fixes to dependencies and pragmas.

3.8.6 [maintenance release]
-----
* Fixed an issue with `DefaultSignatures` being used outside of the appropriate `#ifdef` that caused compilation issues on GHC 7.0.2.
* Generalized the signature of `prism'`
* Added `\_Void` and `only` to `Control.Lens.Prism` and `devoid` to `Control.Lens.Lens`.
* Added `\_Nothing` to `Control.Lens.Prism`.
* Added `devoid` and `united` to `Control.Lens.Lens`.

3.8.5
-----
* Fixed more sporadic issues in doctests, caused by carrying flags from `$setup` between modules.

3.8.4
-----
* Renamed `strippingPrefix` to `prefixed`, `strippingSuffix` to `suffixed`. Left the old names as deprecated aliases.
* Fixed issues with the test suite caused by `doctests` carrying flags from the `$setup` block between modules.
* Benchmarks now use `generic-deriving` rather than `ghc-prim` directly, like the rest of the package.
* Added `Generics.Deriving.Lens`, which is now simply re-exported from `GHC.Generics.Lens`.

3.8.3
-----
* Added `strippingSuffix` and `stripSuffix` to `Data.Data.Lens`
* Added `unpackedBytes` and `unpackedChars` to `Data.ByteString.*.Lens`
* Added `unpacked` to `Data.Text.*.Lens`
* Added `(#)` as an infix form of `review` to ease using a `Prism` like a smart constructor in `Control.Lens.Review`.

3.8.2
-----
* Added a notion of `Handleable(handler, handler_)` to `Control.Exception.Lens` to facilitate constructing a `Handler` from an arbitrary `Fold` or `Prism`.
* Added a notion of `Handler` and `catches` to and `Control.Monad.Error.Lens` to mirror the `Control.Exception` and `Control.Monad.CatchIO` constructions.
* Added additional doctests and documentation.
* Improved error messages and support for types with arguments in `makeFields`.

3.8.1
-----
* Fixed a bug in `makeFields` in hierarchical modules.

3.8.0.2
-------
* Fixed an issue with running the `doctests` test suite when an older version of `semigroups` is installed.

3.8
---
* Overall:
  * Replaced each of the different `SimpleFoo` type aliases with `Foo'` throughout. The various `Simple` aliases can still be found in `Control.Lens.Simple` but are now deprecated.
  * Made sweeping changes to `Iso` and `Prism` and `Indexed` lenses internally. They are now based on `profunctors`. This affects how you use `indexed` in the resulting code and dramatically changed the meaning of `Overloaded`.
  * Generalized combinators to pass through indices unmodified wherever possible and added indexed variants to existing combinators. There are hundreds of these changes and they would swamp this list.
* `Control.Exception.Lens`
  * This module was created to add combinators and prisms that make it possible to work with GHC's extensible exceptions and monad transformer stacks more easily. There are knock-on changes in `Data.Dynamic.Lens`, `System.Exit.Lens`, and `System.IO.Error.Lens`.
* `Control.Lens.At`
  * Moved `At(at)` and `Contains(contains)` and factored out `Ixed(ix)`.
  * Deprecated `_at` and `resultAt`.
  * Removed various `ordinal` and `ix` combinators, which are subsumed by `Ixed(ix)`.
* `Control.Lens.Cons`
  * Consoldiated the various `_head`, `_tail`, `_init` and `_last` traversals that were scattered around the place into a pair of `Cons` and `Snoc` classes that provide `_Cons` and `_Snoc` prisms respectively, and combinators that build on top.
* `Control.Lens.Each`
  * Generalized the signature of `Each` to permit it to provide an `IndexedSetter` for `((->) e)`.
  * `Each` now uses an `Index` type family that is shared with `At`, `Ixed` and `Contains` to indicate these operations are related.
* `Control.Lens.Equality`
  * Added as a stronger form of `Iso` that can be used to safely cast.
  * Added the adverb `simply`, which can be used to simplify the types of most combinators in the library so they only take a simple lens, simple traversal, etc as their first argument instead. e.g. `simply view` forces `a ~ b`, `s ~ t` in the argument to `view`.
* `Control.Lens.Fold`
  * Added `foldr1Of'` and `foldl1Of'`.
  * Added `has` and `hasn't`.
* `Control.Lens.Indexed`
  * The various indexed combinators for each type were distributed to their respective modules. This module grew to encompass the remaining index-specifics.
  * Added `index` and `indices`, and removed `iwhere` and `iwhereOf`. Use `itraversed.indices even` and `bar.indices (>3)` instead.
* `Control.Lens.Internal`
  * This module was exploded into more manageable component modules.
* `Control.Lens.Iso`
  * `Strict(strict)` is now a `Simple Iso`.
  * Added `magma` and `imagma` which can be used to provide a 'debugging view' of a `Traversal`.
* `Control.Lens.Lens`
  * Restructuring split this module out from `Control.Lens.Type` and merged the contents `Control.Lens.IndexedLens`.
* `Control.Lens.Level`
  * This module was created to provide the breadth-first-search Traversals `levels` and `ilevels` which can be used to do (optionally depth-limited) breadth-first searches through arbitrary traversals reaching all leaves at finite depth in finite time. To use these in full accordance with the laws you should restrict yourself to commutative operations and finite containers, but they are useful even in the absence of these properties.
* `Control.Lens.Loupe`
  * In the interest of consistency, the `Loupe` alias has been deprecated in favor of `ALens`.
  * `Loupe` (and `ALens`) are now defined in terms of `Pretext` rather than `Context`. This permits them to be cloned at a reduced cost reducing the call for `ReifiedLens`.
* `Control.Lens.Operators`
  * Added this module for users who insist on qualified use, but want access to the operators. They can `import qualified Control.Lens as Lens` and `import Control.Lens.Operators` unqualified.
* `Control.Lens.Prism`
  * Added `prism'` to construct `SimplePrism`s.
* `Control.Lens.Reified`
  * Consolidated the various `ReifiedFoo` definitions into one module.
* `Control.Lens.Representable`
  * This module was removed. Its functionality may be split out into a separate package, but currently the `linear` package exports is own `Linear.Core` module to provide this functionality. It was taking lots of useful names for little functionality and didn't feel like the rest of the API.
* `Control.Lens.Review`
  * This module now factors the `review` functionality out of `Prism` and exposes `unto`, which is to `review` what `to` is to `view`.
* `Control.Lens.Setter`
  * Added `contramapped` and `argument` for mapping over inputs.
* `Control.Lens.Simple`
  * Removed the infix lens aliases and repurposed the module to house the now deprecated `SimpleFoo` type aliases, which were replaced universally with `Foo'`.
* `Control.Lens.TH`
  * `makeLenses` now generates `Lens'` and `Traversal'` where appropriate
  * Added `makePrisms` as a generalized `makeIso` that automatically generates a `Prism` for each constructor. `makePrisms` generates names with an `_Foo` convention. This was consolidated upon throughout the library to reduce namespace conflicts between prisms and lenses.
  * Added `makeFields`, which generates classes for each individual field in a data type.
  * Added `makeWrapped`, which automatically generates a `Wrapped` instance for a newtype.
* `Control.Lens.Type`
  * This module was repurposed to provide a single home for all the standard lens-like type aliases used when producing lenses. You still need to go to their respective modules to find the types for consuming lens-likes if you want to generate your own lens combinators
* `Control.Lens.Wrapped`
  * Added `wrapped'` and `unwrapped'` for scenarios where you need the help with type inference.
* `Control.Lens.Zipper`
  * Converted `Zipper` to walk a magma based on the original structure and to use indices from indexed traversals when restoring from tape. This also means that when zipping around within a balanced structure with ascending keys `moveTo` can operate in logarithmic time, but required changing the `Zipper` type to add the index type.
* `Data.Bits.Lens`
  * Added `byteAt`.
* `Data.ByteString.Lens`
  * `Data.ByteString.Lazy.Lens` now uses `Int64`-based indexing.
  * The `Traversal` for strict `ByteStrings` now construct a balanced tree up to a given grain size. This permits zipper based seeking to operate in logarithmic time and speeds up many traversals.
* `Numeric.Lens`
  * Created. `base` shows and reads integers at base-2 through base-36. `integral` can be used as a safe `fromInteger`/`toInteger`.

3.7.6 [maintenance release]
-----
* Fixed an issue with the `Complex` `Each` instance.

3.7.5 [maintenance release]
-----
* Fixed an errant `LANGUAGE` pragma

3.7.4 [maintenance release]
-----
* Backported the API for `ALens` and `ALens'` to support `snap` builds on old platforms.

3.7.3 [maintenance release]
-----
* Removed my intra-package dependency upper bounds for my own packages. In particular this enables us to work with `semigroups` 0.9.
* Switched to `transformers-compat` to avoid having unbuilding modules at the top of the documentation, and to ease 3rd party compatibility.
* Updated `Setup.lhs` to be compatible with Cabal 1.17

3.7.2 [maintenance release]
-----
* Bug fix for `Magnify`. It was missing functional dependencies to determine its `k` parameter from `m` or `n`.

3.7.1.2 [maintenance release]
-------
* Made the doctest test suite hide all but the exact versions of packages used to build this package to avoid problems with complicated user environments.
* Removed doctests based on `:t` as they are fragile and break across GHC versions.
* Fixed GHC 7.0.4 compatibility by guarding `DefaultSignatures` in `Control.Lens.Each`.

3.7.1.1 [maintenance release]
-------
* Removed tests that will (likely) fail in the presence of `hashable` 1.2

3.7.1
-----
* Added `preuse`, `preuses` to `Control.Lens.Fold`
* Added `Each(each)` to `Control.Lens.Each` for indexed traversal of potentially monomorphic containers.
* Added `indexing64` and `traversed64` for help with large containers.
* Generalized the type signature of `choosing`.
* Exported `unwrapped` from `Control.Lens.Wrapped`.
* Support for `hashable` 1.2
* Added `(??)` to `Control.Lens.Combinators`.

3.7.0.2
-------
* Fixed flagging for Safe Haskell.
* Fixed examples.
* Cleaned up the statement of the Prism laws.

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
* We've generalized the type of Bazaar and provided generalized variants of `partsOf`, etc. that used it.

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
  Unsafe module and create an instance of `Trustworthy` for your type to cause this behavior, so if you do, it's on your head, not mine. :)
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
* We've relinquished the name `value`.

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
