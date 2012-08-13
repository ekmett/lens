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
