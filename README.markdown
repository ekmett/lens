lens
====

[![Build Status](https://secure.travis-ci.org/ekmett/lens.png?branch=master)](http://travis-ci.org/ekmett/lens)

This package provides families of lenses, isomorphisms, folds, traversals, getters and setters.

These lenses are compatible with those from lens-family, lens-family-core and lens-family-th,
but they provide a great deal of additional flexibility in their composition.

Example
-------

    ghci> :m + Control.Lens Data.Text.Lens
    ghci> anyOf (traverse.text) (=='y') ["hello"^.packed, "goodbye"^.packed]
    True

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
