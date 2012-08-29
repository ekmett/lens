Lens: Lenses, Folds, and Traversals
==================================

[![Build Status](https://secure.travis-ci.org/ekmett/lens.png?branch=master)](http://travis-ci.org/ekmett/lens)

This package provides families of [lenses](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Type.hs), [isomorphisms](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Iso.hs), [folds](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Fold.hs), [traversals](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Traversal.hs), [getters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Getter.hs) and [setters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Setter.hs).

An overview of the [derivation](https://github.com/ekmett/lens/wiki/Derivation) of these types can be found on the [Lens Wiki](https://github.com/ekmett/lens/wiki) along with a brief [Tutorial](https://github.com/ekmett/lens/wiki/Tutorial).

Documentation is available through [github](http://ekmett.github.com/lens) or [hackage](http://hackage.haskell.org/package/lens).

Plated
------

New in version 2.5 is a port of the `Uniplate` API, updated to use `Traversal`. The Data-derived `biplate` and `uniplate` combinators run about 25% faster than the original `uniplate`, and you can use any of the other combinators, since `biplate` and `uniplate` are now just traversals.

Examples
--------

You can read from lenses (or other getters) and they compose in the order an imperative programmer would expect.

```haskell
ghci> :m + Control.Lens
ghci> ("hello",("world","!!!"))^._2._1
"world"
```

You can make getters out of pure functions with `to`.


```haskell
ghci> ("hello",("world","!!!"))^._2._1.to length
5
```

You can write to lenses and these writes can change the type of the container.

```haskell
ghci> _1 .~ "hello" $ ((),"world")
("hello","world)
```

You can let the library automatically derive lenses for fields of your data type

```haskell
import Control.Lens

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo
```

This will automatically generate the following lenses:

```haskell
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b
```

You can also write to setters that target multiple parts of a structure, or their composition with other
lenses or setters.

```haskell
ghci> _1.mapped._2.mapped %~ succ $ ([(42, "hello")],"world")
([(42, "ifmmp")],"world")
```

```haskell
ghci> both *~ 2 $ (1,2)
(2,4)
```

There are combinators for manipulating the current state in a state monad as well

```haskell
fresh :: MonadState Int m => m Int
fresh = id <+= 1
```

Anything you know how to do with a `Foldable` container, you can do with a `Fold`

```haskell
ghci> :m + Data.Char Data.Text.Lens
ghci> allOf (folded.text) isLower ["hello"^.packed, "goodbye"^.packed]
True
```

You can also use this for generic programming:

```haskell
ghci> :m + GHC.Generics.Lens
ghci> anyOf every (=="world") ("hello",(),[(2::Int,"world")])
True
```

Anything you know how to do with a `Traversable` you can do with a `Traversal`.

```haskell
ghci> mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
"hello"
"world"
[(42,5),(56,5)]
```

Many of the lenses supplied are isomorphisms, that means you can use them directly as a lens:

```haskell
ghci> let hello = "hello"^.packed
"hello"
ghci> :t hello
hello :: Text
```

but you can also flip them around and use them as a lens the other way with `from`

```haskell
ghci> hello^.from packed.to length
5
```

You can automatically derive isomorphisms for your own newtypes with `makeIso`. e.g.

```haskell
newtype Neither a b = Neither { _nor :: Either a b } deriving (Show)
makeIso ''Neither
```

will automatically derive

```haskell
neither :: Iso (Neither a b) (Neither c d) (Either a b) (Either c d)
nor :: Iso (Either a b) (Either c d) (Neither a b) (Neither c d)
```

such that

```haskell
from neither = nor
from nor = neither
neither.nor = id
nor.neither = id
```

There is also a fully operational, but simple game of [Pong](https://github.com/ekmett/lens/blob/master/examples/Pong.hs) in the [examples/](https://github.com/ekmett/lens/blob/master/examples/) folder.

Field Guide
-----------

[![Lens Hierarchy](https://s3.amazonaws.com/creately-published/h5nyo9ne1)](https://creately.com/diagram/h5nyo9ne1/LBbRz63yg4yQsTXGLtub1bQU4%3D)

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
