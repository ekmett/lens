0.1
---

* Initial release. Provides the orphan `Plated` instances (for the class in the
  `plated` package) for the `free` and `comonad` type families: `Free`, `FreeT`,
  `F`, `Cofree`, and `CofreeT`. Split out of the `plated` package so that
  depending on the `Plated` class itself does not pull in `free`/`comonad`.
