0.1
---

* Initial release. The `Plated` class, its `Data`-based default for `plate`
  (via a portable `uniplate`), the `[]` and `Data.Tree.Tree` instances, and the
  `Generic`-based `gplate`/`gplate1` (with the `GPlated`/`GPlated1` classes) were
  extracted from the `lens` package's `Control.Lens.Plated` module so that
  libraries can provide `Plated` instances without depending on `lens`.
* Instances that need heavier dependencies (`free`, `comonad`) live in the
  companion `plated-instances` package, keeping this package's footprint to
  `base` and `containers` (a GHC boot library).
