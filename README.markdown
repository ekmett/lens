Lens: Lenses, Folds, and Traversals
==================================

[![Hackage](https://img.shields.io/hackage/v/lens.svg)](https://hackage.haskell.org/package/lens) [![Build Status](https://secure.travis-ci.org/ekmett/lens.svg)](http://travis-ci.org/ekmett/lens) [![Hackage Deps](https://img.shields.io/hackage-deps/v/lens.svg)](http://packdeps.haskellers.com/reverse/lens)

This package provides families of [lenses](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Type.hs), [isomorphisms](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Iso.hs), [folds](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Fold.hs), [traversals](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Traversal.hs), [getters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Getter.hs) and [setters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Setter.hs).

If you are looking for where to get started, [a crash course video](http://youtu.be/cefnmjtAolY?hd=1) on how `lens` was constructed and how to use the basics is available on youtube. It is best watched in high definition to see the slides, but the [slides](http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf) are also available if you want to use them to follow along.

The [FAQ](https://github.com/ekmett/lens/wiki/FAQ), which provides links to a large number of different resources for learning about lenses and an overview of the [derivation](https://github.com/ekmett/lens/wiki/Derivation) of these types can be found on the [Lens Wiki](https://github.com/ekmett/lens/wiki) along with a brief [overview](https://github.com/ekmett/lens/wiki/Overview) and some [examples](https://github.com/ekmett/lens/wiki/Examples).

Documentation is available through [github](http://ekmett.github.com/lens/frames.html) (for HEAD) or [hackage](http://hackage.haskell.org/package/lens) for the current and preceding releases.

Field Guide
-----------

[![Lens Hierarchy](https://raw.githubusercontent.com/wiki/ekmett/lens/images/Hierarchy.png)](https://creately.com/diagram/h5nyo9ne1/QZ9UBOtw4AJWtmAKYK3wT8Mm1HM%3D)


Examples
--------

(See [`wiki/Examples`](https://github.com/ekmett/lens/wiki/Examples))

First, import `Control.Lens`.

```haskell
ghci> import Control.Lens
```

Now, you can read from lenses

```haskell
ghci> ("hello","world")^._2
"world"
```

and you can write to lenses.

```haskell
ghci> set _2 42 ("hello","world")
("hello",42)
```

Composing lenses for reading (or writing) goes in the order an imperative programmer would expect, and just uses `(.)` from the `Prelude`.

```haskell
ghci> ("hello",("world","!!!"))^._2._1
"world"
```

```haskell
ghci> set (_2._1) 42 ("hello",("world","!!!"))
("hello",(42,"!!!"))
```

You can make a `Getter` out of a pure function with `to`.

```haskell
ghci> "hello"^.to length
5
```

You can easily compose a `Getter` with a `Lens` just using `(.)`. No explicit coercion is necessary.

```haskell
ghci> ("hello",("world","!!!"))^._2._2.to length
3
```

As we saw above, you can write to lenses and these writes can change the type of the container. `(.~)` is an infix alias for `set`.

```haskell
ghci> _1 .~ "hello" $ ((),"world")
("hello","world")
```

Conversely `view`, can be used as a prefix alias for `(^.)`.

```haskell
ghci> view _2 (10,20)
20
```

There are a large number of other lens variants provided by the library, in particular a `Traversal` generalizes `traverse` from `Data.Traversable`.

We'll come back to those later, but continuing with just lenses:

You can let the library automatically derive lenses for fields of your data type

```haskell
data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo
```

This will automatically generate the following lenses:

```haskell
bar, baz :: Lens' (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b
```

A `Lens` takes 4 parameters because it can change the types of the whole when you change the type of the part.

Often you won't need this flexibility, a `Lens'` takes 2 parameters, and can be used directly as a `Lens`.

You can also write to setters that target multiple parts of a structure, or their composition with other
lenses or setters. The canonical example of a setter is 'mapped':

```haskell
mapped :: Functor f => Setter (f a) (f b) a b
```

`over` is then analogous to `fmap`, but parameterized on the Setter.

```haskell
ghci> fmap succ [1,2,3]
[2,3,4]
ghci> over mapped succ [1,2,3]
[2,3,4]
```

The benefit is that you can use any `Lens` as a `Setter`, and the composition of setters with other setters or lenses using `(.)` yields
a `Setter`.

```haskell
ghci> over (mapped._2) succ [(1,2),(3,4)]
[(1,3),(3,5)]
```

`(%~)` is an infix alias for 'over', and the precedence lets you avoid swimming in parentheses:

```haskell
ghci> _1.mapped._2.mapped %~ succ $ ([(42, "hello")],"world")
([(42, "ifmmp")],"world")
```

There are a number of combinators that resemble the `+=`, `*=`, etc. operators from C/C++ for working with the monad transformers.

There are `+~`, `*~`, etc. analogues to those combinators that work functionally, returning the modified version of the structure.

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

You can also use this for generic programming. Combinators are included that are based on Neil Mitchell's `uniplate`, but which
have been generalized to work on or as lenses, folds, and traversals.

```haskell
ghci> :m + Data.Data.Lens
ghci> anyOf biplate (=="world") ("hello",(),[(2::Int,"world")])
True
```

As alluded to above, anything you know how to do with a `Traversable` you can do with a `Traversal`.

```haskell
ghci> mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
"hello"
"world"
[(42,5),(56,5)]
```

Moreover, many of the lenses supplied are actually isomorphisms, that means you can use them directly as a lens or getter:

```haskell
ghci> let hello = "hello"^.packed
"hello"
ghci> :t hello
hello :: Text
```

but you can also flip them around and use them as a lens the other way with `from`!

```haskell
ghci> hello^.from packed.to length
5
```

You can automatically derive isomorphisms for your own newtypes with `makePrisms`. e.g.

```haskell
newtype Neither a b = Neither { _nor :: Either a b } deriving (Show)
makePrisms ''Neither
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

There are also a couple of hundred examples distributed throughout the haddock documentation.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
