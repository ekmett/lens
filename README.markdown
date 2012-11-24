Lens: Lenses, Folds, and Traversals
==================================

[![Build Status](https://secure.travis-ci.org/ekmett/lens.png?branch=master)](http://travis-ci.org/ekmett/lens)

This package provides families of [lenses](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Type.hs), [isomorphisms](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Iso.hs), [folds](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Fold.hs), [traversals](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Traversal.hs), [getters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Getter.hs) and [setters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Setter.hs).

An overview of the [derivation](https://github.com/ekmett/lens/wiki/Derivation) of these types can be found on the [Lens Wiki](https://github.com/ekmett/lens/wiki) along with a brief [Overview](https://github.com/ekmett/lens/wiki/Overview).

Documentation is available through [github](http://ekmett.github.com/lens/frames.html) or [hackage](http://hackage.haskell.org/package/lens).

Field Guide
-----------

[![Lens Hierarchy](https://s3.amazonaws.com/creately-published/h5nyo9ne1)](https://creately.com/diagram/h5nyo9ne1/LBbRz63yg4yQsTXGLtub1bQU4%3D)

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

You can make a `Getter` out of a pure functions with `to`.

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
("hello","world)
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
import Control.Lens

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo
```

This will automatically generate the following lenses:

```haskell
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b
```

A `Lens` takes 4 parameters because it can change the types of the whole when you change the type of the part.

Often you won't need this flexibility, a `Simple Lens` takes 2 parameters, and can be used directly as a `Lens`.

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

There are also a couple of hundred examples distributed throughout the haddock documentation.

Operators
=========

(See [`wiki/Operators`](https://github.com/ekmett/lens/wiki/Operators))

<table>
<thead>
<tr>
  <th>Combinator(s)</th>
  <th>w/ Result</th>
  <th>Stateful</th>
  <th>w/ Result</th>
  <th>Notes</th>
</tr>
</thead>
<tbody>
<tr><th colspan=5><a href="http://ekmett.github.com/lens/Control-Lens.html">Control.Lens</a></th></tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:view"><code>view</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:views"><code>views</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:-94-."><code>^.</code></a></td>
  <td/>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:use"><code>use</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:uses"><code>uses</code></a></td>
  <td/>
  <td>View target(s). <a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:query"><code>query</code></a> works like <a href="http://ekmett.github.com/lens/Control-Lens-Getter.html#v:use"><code>use</code></a> over a <code>MonadReader</code></td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:set"><code>set</code></a>, <a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:.-126-"><code>.~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-60-.-126-"><code>&lt;.~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:.-61-"><code>.=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:assign"><code>assign</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-60-.-61-"><code>&lt;.=</code></a></td>
  <td>Replace target(s). <a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--60-.-126-"><code>&lt;&lt;.~</code> and
      <a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--60-.-61-"><code>&lt;&lt;.=</code></a>
      return the old value</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:over"><code>over</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:mapOf"><code>mapOf</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-37--126-"><code>%~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--37--126-"><code>&lt;%~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-37--61-"><code>%=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--37--61-"><code>&lt;%=</code></td>
  <td>Update target(s). <a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--60--37--126-"><code>&lt;&lt;%~</code> and
      <a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--60--37--61-"><code>&lt;&lt;%=</code></a>
      return the old value</td>
</tr>
<tr>
  <td><code>id</code>,<a href="http://ekmett.github.com/lens/Control-Lens-Traversal.html#v:traverseOf"><code>traverseOf</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-37--37--126-"><code>%%~</code></a></td>
  <td/>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-37--37--61-"><code>%%=</code></a></td>
  <td/>
  <td>Update target(s) with an <code>Applicative</code> or auxillary result</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-43--126-"><code>+~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--43--126-"><code>&lt;+~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-43--61-"><code>+=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--43--61-"><code>&lt;+=</code></td>
  <td>Add to target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-45--126-"><code>-~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--45--126-"><code>&lt;-~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-45--61-"><code>-=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--45--61-"><code>&lt;-=</code></td>
  <td>Subtract from target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-42--126-"><code>*~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--42--126-"><code>&lt;*~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-42--61-"><code>*=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--42--61-"><code>&lt;*=</code></td>
  <td>Multiply target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-47--47--126-"><code>//~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--47--47--126-"><code>&lt;//~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-47--47--61-"><code>//=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--47--47--61-"><code>&lt;//=</code></td>
  <td>Divide target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-94--126-"><code>^~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--94--126-"><code>&lt;^~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-94--61-"><code>^=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--94--61-"><code>&lt;^=</code></td>
  <td>Raise target(s) to a non-negative <code>Integral</code> power</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-94--94--126-"><code>^^~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--94--94--126-"><code>&lt;^^~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-94--94--61-"><code>^^=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--94--94--61-"><code>&lt;^^=</code></td>
  <td>Raise target(s) to an <code>Integral</code> power</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-42--42--126-"><code>**~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--42--42--126-"><code>&lt;**~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-42--42--61-"><code>**=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--42--42--61-"><code>&lt;**=</code></td>
  <td>Raise target(s) to an arbitrary power</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-124--124--126-"><code>||~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--124--124--126-"><code>&lt;||~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-124--124--61-"><code>||=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--124--124--61-"><code>&lt;||=</code></td>
  <td>Logically or target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-38--38--126-"><code>&amp;&amp;~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--38--38--126-"><code>&lt;&amp;&amp;~</code></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Setter.html#v:-38--38--61-"><code>&amp;&amp;=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Type.html#v:-60--38--38--61-"><code>&lt;&amp;&amp;=</code></td>
  <td>Logically and target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Fold.html#v:headOf"><code>headOf</code>,<a href="http://ekmett.github.com/lens/Control-Lens-Fold.html#v:-94--63-"><code>^?</code></a></td>
  <td/><td/><td/>
  <td>Return <code>Just</code> the first target or <code>Nothing</code></td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Fold.html#v:toListOf"><code>toListOf</code>,<a href="http://ekmett.github.com/lens/Control-Lens-Fold.html#v:-94-.."><code>^..</code></a></td>
  <td/><td/><td/>
  <td>Return a list of the target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Action.html#v:perform"><code>perform</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-Action.html#v:performs"><code>performs</code></a><a href="http://ekmett.github.com/lens/Control-Lens-Action.html#v:-94-!"><code>^!</code></a></td>
  <td/>
  <td/>
  <td/>
  <td>Perform monadic action(s)</td>
</tr>
<tr><th colspan=5><a href="http://ekmett.github.com/lens/Control-Lens.html">Control.Lens</a> (Indexed)</th></tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-IndexedSetter.html#v:iover"><code>iover</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-IndexedSetter.html#v:imapOf"><code>imapOf</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-IndexedSetter.html#v:-37--64--126-"><code>%@~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-IndexedLens.html#v:-60--37--64--126-"><code>&lt;%@~</code></td>
  <td><a href=http://ekmett.github.com/lens/Control-Lens-IndexedSetter.html#v:-37--64--126-"><code>%@=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-IndexedLens.html#v:-60--37--64--61-"><code>&lt;%@=</code></td>
  <td>Update target(s) with access to the index.</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-Indexed.html#v:withIndex"><code>withIndex</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-IndexedTraversal.html#v:itraverseOf"><code>itraverseOf</code></a>,<a href="http://ekmett.github.com/lens/Control-Lens-IndexedLens.html#v:-37--37--64--126-"><code>%%@~</code></a></td>
  <td/>
  <td><a href="http://ekmett.github.com/lens/Control-Lens-IndexedLens.html#v:-37--37--64--61-"><code>%%@=</code></a></td>
  <td/>
  <td>Update target(s) with an <code>Applicative</code> or auxillary result with access to the index.</td>
</tr>
<tr><th colspan=5><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html">Data.Bits.Lens</a></th></tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-124--126-"><code>.|.~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-60--124--126-"><code>&lt;.|.~</code></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-124--61-"><code>.|.=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-60--124--61-"><code>&lt;.|.=</code></td>
  <td>Bitwise or target(s)</td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-38--126-"><code>.&amp;.~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-60--38--126-"><code>&lt;.&amp;.~</code></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-38--61-"><code>.&amp;.=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Bits-Lens.html#v:-60--38--61-"><code>&lt;.&amp;.=</code></td>
  <td>Bitwise and target(s)</td>
</tr>
<tr><th colspan=5><a href="http://ekmett.github.com/lens/Data-Monoid-Lens.html">Data.Monoid.Lens</a></th></tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/Data-Monoid-Lens.html#v:-60--62--126-"><code>&lt;&gt;~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Monoid-Lens.html#v:-60--60--62--126-"><code>&lt;&lt;&gt;~</code></td>
  <td><a href="http://ekmett.github.com/lens/Data-Monoid-Lens.html#v:-60--62--61-"><code>&lt;&gt;=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/Data-Monoid-Lens.html#v:-60--60--62--61-"><code>&lt;&lt;&gt;=</code></td>
  <td><code>mappend</code> to the target monoidal value(s)</td>
</tr>
<tr><th colspan=5><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html">System.FilePath.Lens</a></th></tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--47--62--126-"><code>&lt;/&gt;~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--60--47--62--126-"><code>&lt;&lt;/&gt;~</code></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--47--62--61-"><code>&lt;/&gt;=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--60--47--62--61-"><code>&lt;&lt;/&gt;=</code></td>
  <td>Append a relative path to a <code>FilePath</code></td>
</tr>
<tr>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60-.-62--126-"><code>&lt;.&gt;~</code></a></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--60-.-62--126-"><code>&lt;&lt;.&gt;~</code></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60-.-62--61-"><code>&lt;.&gt;=</code></a></td>
  <td><a href="http://ekmett.github.com/lens/System-FilePath-Lens.html#v:-60--60-.-62--61-"><code>&lt;&lt;.&gt;=</code></td>
  <td>Append a file extension to a <code>FilePath</code></td>
</tr>
</tbody>
</table>

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
