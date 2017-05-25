{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- @
-- import Control.Lens
-- 
-- data FooBar a
--   = Foo { _x :: ['Int'], _y :: a }
--   | Bar { _x :: ['Int'] }
-- 'makeLenses' ''FooBar
-- @
--
-- This defines the following lenses:
--
-- @
-- x :: 'Lens'' (FooBar a) ['Int']
-- y :: 'Traversal' (FooBar a) (FooBar b) a b
-- @
--
-- You can then access the value of @_x@ with ('^.'), the value of @_y@ –
-- with ('^?') or ('^?!') (since it can fail), set the values with ('.~'),
-- modify them with ('%~'), and use almost any other combinator that is
-- re-exported here on those fields.
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, the simpler type signatures you might want to
-- pretend the combinators have are specified as well.
--
-- More information on how to use lenses is available on the lens wiki:
--
-- <http://github.com/ekmett/lens/wiki>
--
-- <<Hierarchy.png>>
----------------------------------------------------------------------------
module Control.Lens
  ( module Control.Lens.At
  , module Control.Lens.Cons
  , module Control.Lens.Each
  , module Control.Lens.Empty
  , module Control.Lens.Equality
  , module Control.Lens.Fold
  , module Control.Lens.Getter
  , module Control.Lens.Indexed
  , module Control.Lens.Iso
  , module Control.Lens.Lens
  , module Control.Lens.Level
  , module Control.Lens.Plated
  , module Control.Lens.Prism
  , module Control.Lens.Reified
  , module Control.Lens.Review
  , module Control.Lens.Setter
#ifndef DISABLE_TEMPLATE_HASKELL
  , module Control.Lens.TH
#endif
  , module Control.Lens.Traversal
  , module Control.Lens.Tuple
  , module Control.Lens.Type
  , module Control.Lens.Wrapped
  , module Control.Lens.Zoom

  -- * Tutorial
  -- $tutorial

  -- * Motivation
  -- $motivation

  -- * Lenses
  -- $lenses

  -- * Accessor notation
  -- $accessors

  -- * First-class
  -- $firstclass

  -- * Traversals
  -- $traversals

  -- * Types
  -- $types

  -- * Drawbacks
  -- $drawbacks

  -- * Conclusion
  -- $conclusion
  ) where

import Control.Lens.At
import Control.Lens.Cons
import Control.Lens.Each
import Control.Lens.Empty
import Control.Lens.Equality
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Level
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Reified
import Control.Lens.Review
import Control.Lens.Setter
#ifndef DISABLE_TEMPLATE_HASKELL
import Control.Lens.TH
#endif
import Control.Lens.Traversal
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped
import Control.Lens.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif

{- $tutorial
    This @lens@ tutorial targets Haskell beginners and assumes only basic
    familiarity with Haskell.  By the end of this tutorial you should:

    * understand what problems the @lens@ library solves,

    * know when it is appropriate to use the @lens@ library,

    * be proficient in the most common @lens@ idioms,

    * understand the drawbacks of using lenses, and:

    * know where to look if you wish to learn more advanced tricks.
-}

-- $motivation
--
--     The simplest problem that the @lens@ library solves is updating deeply
--     nested records.  Suppose you had the following nested Haskell data types:
-- 
-- > data Atom = Atom { _element :: String, _point :: Point }
-- >
-- > data Point = Point { _x :: Double, _y :: Double }
-- 
--     If you wanted to increase the @x@ coordinate of an `Atom` by one unit, you
--     would have to write something like this in Haskell:
-- 
-- > shiftAtomX :: Atom -> Atom
-- > shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)
-- 
--     This unpacking and repacking of data types grows increasingly difficult the
--     more fields you add to each data type or the more deeply nested your data
--     structures become.
-- 
--     The @lens@ library solves this problem by letting you instead write:
-- 
-- > -- atom.hs
-- >
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Control.Lens hiding (element)
-- >
-- > data Atom = Atom { _element :: String, _point :: Point } deriving (Show)
-- >
-- > data Point = Point { _x :: Double, _y :: Double } deriving (Show)
-- >
-- > makeLenses ''Atom
-- > makeLenses ''Point
-- >
-- > shiftAtomX :: Atom -> Atom
-- > shiftAtomX = over (point . x) (+ 1)
-- 
--     Let's convince ourselves that this works:
-- 
-- > >>> :load atom.hs
-- > >>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
-- > >>> shiftAtomX atom
-- > Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}}
-- 
--     The above solution does not change no matter how many fields we add to
--     @Atom@ or @Point@.
-- 
--     Now suppose that we added yet another data structure:
-- 
-- > data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)
-- 
--     We could shift an entire @Molecule@ by writing:
-- 
-- > makeLenses ''Molecule
-- >
-- > shiftMoleculeX :: Molecule -> Molecule
-- > shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)
-- 
--     Again, this works the way we expect:
-- 
-- > >>> :load atom.hs
-- > >>> let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
-- > >>> let atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
-- > >>> let molecule = Molecule { _atoms = [atom1, atom2] }
-- > >>> shiftMoleculeX molecule  -- Output formatted for clarity
-- > Molecule
-- >     { _atoms =
-- >         [ Atom { _element = "C", _point = Point { _x = 2.0, _y = 2.0 } }
-- >         , Atom { _element = ")", _point = Point { _x = 4.0, _y = 4.0 } }
-- >         ]
-- >     }
-- 
--     Many people stumble across lenses while trying to solve this common problem
--     of working with data structures with a large number of fields or deeply
--     nested values.  These sorts of situations arise commonly in:
-- 
--     * games with complex and deeply nested state
-- 
--     * scientific data formats
-- 
--     * sensor or instrument output
-- 
--     * web APIs
-- 
--     * XML and JSON
-- 
--     * enterprise code where data structures can have tens, hundreds, or even
--       thousands of fields (true story!)

{- $lenses
    You might have some basic questions like:

    /Question:/ What is a lens?

    /Answer:/ A lens is a first class getter and setter

    We already saw how to use lenses to update values using `over`, but we can
    also use lenses to retrieve values using `view`:

> >>> :load atom.hs
> >>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
> >>> view (point . x) atom
> 1

    In other words, lenses package both \"get\" and \"set\" functionality into
    a single value (the lens).  You could pretend that a lens is a record
    with two fields:

> data Lens a b = Lens
>     { view :: a -> b
>     , over :: (b -> b) -> (a -> a)
>     }

    That's not how lenses are actually implemented, but it's a useful
    starting intuition.

    /Question:/ What is the type of a lens?

    /Answer:/ We used two lenses in the above @Atom@ example, with these types:

> point :: Lens' Atom  Point
> x     :: Lens' Point Double

    The @point@ lens contains all the information we need to get or set the
    @_point@ field of the @Atom@ type (which is a `Point`).  Similarly, the @x@
    lens contains all the information we need to get or set the @_x@ field of
    the @Point@ data type (which is a `Double`).

    The convention for the `Lens'` type parameters is:

> --    +-- Bigger type
> --    |
> --    v
> Lens' bigger smaller
> --           ^
> --           |
> --           +--  Smaller type within the bigger type

    The actual definition of `Lens'` is:

> type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

    You might wonder how you can fit both getter and setter functionality in
    a single value like this.  The trick is that we get to pick what `Functor`
    we specialize @f@ to and depending on which `Functor` we pick we get
    different features.

    For example, if you pick @(f = `Identity`)@:

> type ASetter' a b   = (b -> Identity b) -> (a -> Identity a)
>
> -- ... equivalent to: (b ->          b) -> (a ->          a)

    ... you can build an `over`-like function.

    Similarly, if you pick @(f = `Const` b)@:

> type Getting b a b  = (b -> Const b b) -> (a -> Const b b)
>
> -- ... equivalent to: (b ->       b  ) -> (a ->       b  )
>
> -- ... equivalent to:                     (a ->       b  )

    ... you can build a `view`-like function.

    Those are not the only two `Functor`s we can pick.  In fact, we can do a
    lot more with lenses than just get and set values, but those are the two
    most commonly used features.

    /Question:/ How do I create lenses?

    /Answer:/ You can either auto-generate them using Template Haskell or
    create them by hand

    In our @Atom@ example, we auto-generated the lenses using Template Haskell,
    like this:

> makeLenses ''Atom
> makeLenses ''Point

    This created four lenses of the following types:

> element :: Lens' Atom String
> point   :: Lens' Atom Point
> x       :: Lens' Point Double
> y       :: Lens' Point Double

    `makeLenses` creates one lens per field prefixed with an underscore.  The
    lens has the same name as the field without the underscore.

    However, sometimes Template Haskell is not an option, so we can also use
    the `lens` utility function to build lenses.  This utility has type:

> lens :: (a -> b) -> (b -> a -> a) -> Lens' a b

    The first argument is a \"getter\" (a way to extract a @\'b\'@ from an
    @\'a\'@).  The second argument is a \"setter\" (given a @b@, update an
    @a@).  The result is a `Lens'` built from the getter and setter.  You would
    use `lens` like this:

> point :: Lens' Atom Point
> point = lens _point (\newPoint atom -> atom { _point = newPoint })

    You can even define lenses without incurring a dependency on the @lens@
    library.  Remember that lenses are just higher-order functions over
    `Functor`s, so we could instead write:

> -- point :: Lens' Atom Point
> point :: Functor f => (Point -> f Point) -> Atom -> f Atom
> point k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))

    This means that you can provide lenses for your library's types without
    depending on the @lens@ library.  All you need is the `fmap` function,
    which is provided by the Haskell Prelude.

    /Question:/ How do I combine lenses?

    /Answer:/ You compose them, using function composition (Yes, really!)

    You can think of the function composition operator as having this type:

> (.) :: Lens' a b -> Lens' b c -> Lens' a c

    We can compose lenses using function composition because `Lens'` is a
    type synonym for a higher-order function:

> type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

    So under the hood we are composing two higher-order functions to get back a
    new higher-order function:

> (.) :: Functor f
>     => ((b -> f b) -> (a -> f a))
>     -> ((c -> f c) -> (b -> f b))
>     -> ((c -> f c) -> (a -> f a))

    In our original @Atom@ example, we composed the @point@ and @x@ lenses to
    create a new composite lens:

> point     :: Lens' Atom Point
> x         :: Lens' Point Double
>
> point . x :: Lens' Atom Double

    This composite lens lets us get or set the @x@ coordinate of an @Atom@.
    We can use `over` and `view` on the composite `Lens'` and they will behave
    exactly the way we expect:

> view (point . x) :: Atom -> Double
>
> over (point . x) :: (Double -> Double) -> (Atom -> Atom)

    /Question:/ How do I consume lenses?

    /Answer:/ Using `view`, `set` or `over`

    Here are their types:

> view :: Lens' a b -> a -> b
>
> over :: Lens' a b -> (b -> b) -> a -> a
>
> set  :: Lens' a b ->       b  -> a -> a
> set lens b = over lens (\_ -> b)

    `view` and `over` are the two fundamental functions on lenses.  `set` is
    just a special case of `over`.

    `view` and `over` are fundamental because they distribute over lens
    composition:

> view (lens1 . lens2) = (view lens2) . (view lens1)
>
> view id = id

> over (lens1 . lens2) = (over lens1) . (over lens2)
>
> over id = id

    /Question:/ What else do I need to know?

    /Answer:/ That's pretty much it!

    For 90% of use cases, you just:

    * Create lenses (using `makeLens`, `lens` or plain-old `fmap`)

    * Compose them (using (`.`))

    * Consume them (using `view`, `set`, and `over`)

    You could actually stop reading here if you are in a hurry since this
    covers the overwhelmingly common use case for the library.  On the other
    hand, keep reading if you would like to learn additional tricks and
    features.
-}

{- $accessors
    You might be used to object-oriented languages where you could retrieve a
    nested field using:

> atom.point.x

    You can do almost the exact same thing using the @lens@ library, except
    that the first dot will have a @^@ right before the dot:

> >>> :load atom.hs
> >>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
> >>> atom^.point.x
> 1.0

    You can better understand why this works, by adding whitespace and
    explicit parentheses:

> atom ^. (point . x)

    This trick uses (`^.`), which is an infix operator equivalent to `view`:

> (^.) :: a -> Lens' a b -> b
> x ^. l = view l x

    ... and you just keep adding dots after that for each lens you compose.
    This gives the appearance of object-oriented accessors if you omit the
    whitespace around the operators.
-}

{- $firstclass
    Lenses are \"first class\" values, meaning that you can manipulate them
    using ordinary functional programming techniques.  You can take them as
    inputs, return them as outputs, or stick them in data structures.  Anything
    goes!

    For example, suppose we don't want to define separate shift functions for
    @Atom@s and @Molecule@s:

> shiftAtomX :: Atom -> Atom
> shiftAtomX = over (point . x) (+ 1)

> shiftMoleculeX :: Molecule -> Molecule
> shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

    We can instead unify them into a single function by parametrizing the
    shift function on the lens:

> shift lens = over lens (+ 1)

    This lets us write:

> shift (point . x) :: Atom -> Atom
>
> shift (atoms . traverse . point . x) :: Molecule -> Molecule

    Even better, we can define synonyms for our composite lenses:

> atomX :: Lens' Atom Double
> atomX = point . x
>
> -- We'll learn what `Traversal` means shortly
> moleculeX :: Traversal' Molecule Double
> moleculeX = atoms . traverse . point . x

    Now we can write code almost identical to the original code:

> shift atomX :: Atom -> Atom
>
> shift moleculeX :: Molecule -> Molecule

    ... but we also get several other utilities for free:

> set atomX :: Double -> Atom -> Atom
>
> set moleculeX :: Double -> Molecule -> Molecule
>
> view atomX :: Atom -> Double
>
> -- We can't use `view` for `Traversal'`s.  Read on to find out why
> toListOf moleculeX :: Molecule -> [Double]

    That's much more reusable, but you might wonder what this `Traversal'` and
    `toListOf` business is all about.
-}

-- $traversals
--     /Question:/ What is a traversal?
-- 
--     /Answer:/ A first class getter and setter for an arbitrary number of values
-- 
--     A traversal lets you get all the values it points to as a list and it also
--     lets you update or set all the values it points to.  Think of a traversal
--     as a record with two fields:
-- 
-- > data Traversal' a b = Traversal'
-- >     { toListOf :: a -> [b]
-- >     , over     :: (b -> b) -> (a -> a)
-- >     }
-- 
--     That's not how traversals are actually implemented, but it's a useful
--     starting intuition.
-- 
--     We can still use `over` and `set` (a special case of `over`) with a
--     traversal, but instead of `view` we use `toListOf`.
-- 
--     /Question:/ What is the type of a traversal?
-- 
--     /Answer:/ We used one traversal in the above @Molecule@ example:
-- 
-- > moleculeX :: Traversal' Molecule Double
-- 
--     This `Traversal'` lets us get or set an arbitrary number of x coordinates,
--     each of which is a `Double`.  There could be less than one x coordinate
--     (i.e. 0 coordinates) or more than one x coordinate.  Contrast this with a
--     `Lens'` which can only get or set exactly one value.
-- 
--     Like `Lens'`, `Traversal'` is a type synonym for a higher-order function:
-- 
-- > type Traversal' a b = forall f . Applicative f => (b -> f b) -> (a -> f a)
-- >
-- > type Lens'      a b = forall f . Functor     f => (b -> f b) -> (a -> f a)
-- 
--     Notice that the only difference between a `Lens'` and a `Traversal'` is the
--     type class constraint.  A `Lens'` has a `Functor` constraint and
--     `Traversal'` has an `Applicative` constraint.  This means that any `Lens'`
--     is automatically also a valid `Traversal'` (since `Functor` is a superclass
--     of `Applicative`).
-- 
--     Since every `Lens'` is a `Traversal'`, all of our example lenses also
--     double as traversals:
-- 
-- > atoms   :: Traversal' Molecule [Atom]
-- > element :: Traversal' Atom     String
-- > point   :: Traversal' Atom     Point
-- > x       :: Traversal' Point    Double
-- > y       :: Traversal' Point    Double
-- 
--     We actually used yet another `Traversal'`, which was `traverse` (from
--     "Data.Traversable"):
-- 
-- > traverse :: Traversable t => Traversal' (t a) a
-- 
--     This works because the `Traversal'` type synonym expands out to:
-- 
-- > traverse :: (Applicative f, Traversable t) => (a -> f a) -> t a -> f (t a)
-- 
--     ... which is exactly the traditional type signature of `traverse`:
-- 
--     In our @Molecule@ example, we were using the special case where @t = []@:
-- 
-- > traverse :: Traversal' [a] a
-- 
--     In Haskell, you can derive `Functor`, `Data.Foldable.Foldable` and
--     `Traversable` for many data types using the @DeriveFoldable@ and
--     @DeriveTraversable@ extensions.  This means that you can autogenerate a
--     valid `traverse` for these data types:
-- 
-- > {-# LANGUAGE DeriveFoldable    #-}
-- > {-# LANGUAGE DeriveFunctor     #-}
-- > {-# LANGUAGE DeriveTraversable #-}
-- >
-- > import Control.Lens
-- > import Data.Foldable
-- >
-- > data Pair a = Pair a a deriving (Functor, Foldable, Traversable)
-- 
--     We could then use `traverse` to navigate from `Pair` to its two children:
-- 
-- > traverse :: Traversal' (Pair a) a
-- >
-- > over traverse :: (a -> a) -> (Pair a -> Pair a)
-- >
-- > over traverse (+ 1) (Pair 3 4) = Pair 4 5
-- 
--     /Question:/ How do I create traversals?
-- 
--     /Answer:/ There are three main ways to create primitive traversals:
-- 
--     * `traverse` is a `Traversal'` that you get for any type that implements
--       `Traversable`
-- 
--     * Every `Lens'` will also type-check as a `Traversal'`
-- 
--     * You can use Template Haskell to generate `Traversal'`s using `makePrisms`
--       since every `Prism'` is also a `Traversal'` (not covered in this
--       tutorial)
-- 
--     /Question:/ How do I combine traversals?
-- 
--     /Answer:/ You compose them, using function composition
-- 
--     You can think of the function composition operator as having this type:
-- 
-- > (.) :: Traversal' a b -> Traversal' b c -> Traversal' a c
-- 
--     We can compose traversals using function composition because a
--     `Traversal'` is a type synonym for a higher-order function:
-- 
-- > type Traversal' a b = forall f . Applicative f => (b -> f b) -> (a -> f a)
-- 
--     So under the hood we are composing two functions to get back a new
--     function:
-- 
-- > (.) :: Applicative f
-- >     => ((b -> f b) -> (a -> f a))
-- >     -> ((c -> f c) -> (b -> f b))
-- >     -> ((c -> f c) -> (a -> f a))
-- 
--     In our original @Molecule@ example, we composed four `Traversal'`s
--     together to create a new `Traversal'`:
-- 
-- > -- Remember that `atoms`, `point`, and `x` are also `Traversal'`s
-- > atoms                        :: Traversal' Molecule [Atom]
-- > traverse                     :: Traversal' [Atom]   Atom
-- > point                        :: Traversal' Atom     Point
-- > x                            :: Traversal' Point    Double
-- >
-- > -- Now compose them
-- > atoms                        :: Traversal' Molecule [Atom]
-- > atoms . traverse             :: Traversal' Molecule Atom
-- > atoms . traverse . point     :: Traversal' Molecule Point
-- > atoms . traverse . point . x :: Traversal' Molecule Double
-- 
--     This composite traversal lets us get or set the @x@ coordinates of a
--     @Molecule@.
-- 
-- > over (atoms . traverse . point . x)
-- >     :: (Double -> Double) -> (Molecule -> Molecule)
-- >
-- > toListOf (atoms . traverse . point . x) :: Molecule -> [Double]
-- 
--     /Question:/ How do I consume traversals?
-- 
--     /Answer:/ Using `toListOf`, `set` or `over`
-- 
--     Here are their types:
-- 
-- > toListOf :: Lens' a b -> a -> [b]
-- >
-- > over :: Traversal' a b -> (b -> b) -> a -> a
-- >
-- > set  :: Traversal' a b ->       b  -> a -> a
-- > set traversal b = over traversal (\_ -> b)
--
--     `toListOf` distributes over traversal composition:
-- 
-- > toListOf (traversal1 . traversal2) = (toListOf traversal1) >=> (toListOf traversal2)
-- >
-- > toListOf id = return
--
-- You can also use (`^..`), which is an infix operator equivalent to
-- `toListOf`:
--
-- > >>> Pair 3 4 ^.. traverse
-- > [3,4]

{- $types
    You might wonder why you can use `over` on both a `Lens'` and a
    `Traversal'` but you can only use `view` on a `Lens'`.  We can see why by
    studying the (simplified) type and implementation of `over`:

> over :: ((b -> Identity b) -> (a -> Identity b)) -> (b -> b) -> a -> a
> over setter f x = runIdentity (setter (\y -> Identity (f y)) x)

    To follow the implementation, just step slowly through the types:

> setter :: (b -> Identity b) -> (a -> Identity b)
> f      :: b -> b
> x      :: a

>                      \y -> Identity (f y)     :: b -> Identity b
>              setter (\y -> Identity (f y))    :: a -> Identity a
>              setter (\y -> Identity (f y)) x  ::      Identity a
> runIdentity (setter (\y -> Identity (f y)) x) ::               a

     We can replace @setter@ with @point@ and replace @x@ with @atom@ to see
     that this generates the correct code for updating an atom's point:

>   over point f atom
>
> -- Definition of `over`
> = runIdentity (point (\y -> Identity (f y)) atom)
>
> -- Definition of `point`
> = runIdentity (fmap (\newPoint -> atom { _point = newPoint }) (Identity (f (_point atom)))
>
> -- fmap g (Identity y) = Identity (g y)
> = runIdentity (Identity (atom { _point = f (_point atom) }))
>
> -- runIdentity (Identity z) = z
> = atom { _point = f (_point atom) }

    ... which is exactly what we would have written by hand without lenses.

    The reason `over` works for both `Lens'`es and `Traversal'`s is because
    `Identity` implements both `Functor` and `Applicative`:

> instance Functor     Identity where ...
> instance Applicative Identity where ...

    So both the `Lens'` type and `Traversal'` type synonyms:

> type Traversal' a b = forall f . Applicative f => (b -> f b) -> (a -> f a)
>
> type Lens'      a b = forall f . Functor     f => (b -> f b) -> (a -> f a)

    ... can be specialized to use `Identity` in place of @f@:

> (b -> Identity b) -> (a -> Identity a)

    ... making them valid arguments to `over`.

    Now let's study the (simplified) type and implementation of `view`:

> view :: ((b -> Const b b) -> (a -> Const b a)) -> a -> b
> view getter x = getConst (getter Const x)

    Again, we can walk slowly through the types:

> getter :: (b -> Const b b) -> (a -> Const b a)
> x      :: a

> getter Const              :: a -> Const b a
> getter Const x            ::      Const b a
> getConst (getter Const x) ::            b

    Let's see how this plays out for the @point@ lens:

>   view point atom
>
> -- Definition of `view`
> = getConst (point Const atom)
> 
> -- Definition of `point`
> = getConst (fmap (\newPoint -> atom { _point = newPoint }) (Const (_point atom)))
>
> -- fmap g (Const y) = Const y
> = getConst (Const (_point atom))
>
> -- getConst (Const z) = z
> = _point atom

    ... which is exactly what we would have written by hand without lenses.

    `view` accepts `Lens'`es because `Const` implements `Functor`:

> instance Functor (Const b)

    ... so the `Lens'` type synonym:


> type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

    ... can be specialized to use @(`Const` b)@ in place of @f@:

> (b -> Const b b) -> (a -> Const b b)


    ... making it a valid argument to `view`.

    Interestingly, `Const` implements also `Applicative`, but with a
    constraint:

> instance Monoid b => Applicative (Const b)

    This implies that we *can* use `view` on a `Traversal'`, but only if the
    value that we extract is a `Monoid`.  Let's try this out:

> >>> :load atom.hs
> >>> let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
> >>> let atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
> >>> let molecule = Molecule { _atoms = [atom1, atom2] }
> >>> view (atoms . traverse . element) molecule
> "CO"

    This works because our traversal's result is a `String`:

> atoms . traverse . element :: Traversal' Molecule String

    ... and `String` implements the `Data.Monoid.Monoid` interface.  When you
    try to extract multiple strings using `view` they get flattened together
    into a single `String` using `Data.Monoid.mappend`.

    If you try to extract the element from an empty molecule:

> >>> view (atoms . traverse . element) (Molecule { _atoms = [] })
> ""

    You get the empty string (i.e. `Data.Monoid.mempty`).

    This is why the result of a `Traversal'` needs to be a `Data.Monoid.Monoid`
    when using `view`.  If the `Traversal'` points to more than one value you
    need some way to combine them into a single value (using
    `Data.Monoid.mappend`) and if the `Traversal'` points to less than one
    value you need a default value to return (using `Data.Monoid.mempty`).

    If you try to `view` a `Traversal'` that doesn't point to a
    `Data.Monoid.Monoid`, you will get the following type error:

> >>> view (atoms . traverse . point . x) molecule
>     No instance for (Data.Monoid.Monoid Double)
>       arising from a use of `traverse'
>     Possible fix:
>       add an instance declaration for (Data.Monoid.Monoid Double)
>     In the first argument of `(.)', namely `traverse'
>     In the second argument of `(.)', namely `traverse . point . x'
>     In the first argument of `view', namely
>       `(atoms . traverse . point . x)'

    The compiler complains that `Double` does not implement the
    `Data.Monoid.Monoid` type class, so there is no sensible way to merge all
    the x coordinates that our `Traversal'` points to.  For these cases you
    should use `toListOf` instead.
-}

{- $drawbacks
    Lenses come with trade-offs, so you should use them wisely.

    For example, lenses do not produce the best error messages.  Unless you
    understand how `Traversal'`s work you will probably not understand this
    error message:

> >>> view (atoms . traverse . point . x) molecule
>     No instance for (Data.Monoid.Monoid Double)
>       arising from a use of `traverse'
>     Possible fix:
>       add an instance declaration for (Data.Monoid.Monoid Double)
>     In the first argument of `(.)', namely `traverse'
>     In the second argument of `(.)', namely `traverse . point . x'
>     In the first argument of `view', namely
>       `(atoms . traverse . point . x)'

    Also, lenses increase the learning curve for new Haskell programmers, so
    you should consider avoiding them in tutorial code targeting novice
    Haskell programmers.

    Lenses also add a level of boilerplate to all data types to auto-generate
    lenses and increase compile times.  So for small projects the overhead of
    adding lenses may dwarf the benefits.

    @lens@ is also a library with a large dependency tree.  If you need a
    more light-weight alternative you can use the @lens-simple@ library which
    provides a restricted subset of this library with a much smaller dependency
    tree.  In contrast, this @lens@ library focuses on being \"batteries
    included\" and significantly more general.

    The ideal use case for the @lens@ library is a large project with rich and
    deeply nested types.  In these large projects the benefits of using lenses
    outweigh the costs.
-}

{- $conclusion
    This tutorial covers an extremely small subset of this library.  If you
    would like to learn more, you can begin by skimming the example code in the
    following modules:

    * "Control.Lens.Getter"

    * "Control.Lens.Setter"

    * "Control.Lens.Traversal"

    * "Control.Lens.Tuple"

    * "Control.Lens.Lens"

    * "Control.Lens.Review"

    * "Control.Lens.Prism"

    * "Control.Lens.Iso"

    The documentation for these modules includes several examples to get you
    started and help you build an intuition for more advanced tricks.

    You can also study several long-form examples here:

    <https://github.com/ekmett/lens/tree/master/examples>

    If you would like a broader survey of lens features, then you can check
    out these tutorials:

    * <https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial A little lens starter tutorial> - Introduces
Prisms, Isos and JSON functionality

    * <http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html Program imperatively using Haskell lenses> - Illustrates lens support for stateful code
-}
