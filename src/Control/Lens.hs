{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
--
-- This package provides lens families, setters, getters, traversals and folds that
-- can all be composed automatically with each other (and other lenses from
-- other van Laarhoven lens libraries) using @(.)@ from Prelude, while
-- reducing the complexity of the API.
--
-- For a longer description and motivation of why you should care about lens families,
-- see <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Note: If you merely want your library to /provide/ lenses you may not
-- have to actually import /any/ lens library. For, say, a
-- @'Simple' 'Lens' Bar Foo@, just export a function with the signature:
--
-- > foo :: Functor f => (Foo -> f Foo) -> Bar -> f Bar
--
-- and then you can compose it with other lenses with @(.)@ without needing
-- anything from this library at all.
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- > import Control.Lens.TH
-- > data Foo a = Foo { _fooArgs :: [String], _fooValue :: a }
-- > makeLenses ''Foo
--
-- This defines the following lenses:
--
-- > fooArgs :: Simple Lens (Foo a) [String]
-- > fooValue :: Lens (Foo a) (Foo b) a b
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, I've tried to list the simpler type signatures
-- you might want to pretend the combinators have.
--
----------------------------------------------------------------------------
module Control.Lens
  (
  -- * Lenses
    Lens

  -- * "Simple" Lenses
  , Simple

  -- ** Constructing Lenses
  , lens
  , iso
  , clone

  -- * Getters
  , Getter
  , to

  -- ** Getting Values
  , view
  , views
  , (^.), (^$)

  -- * Setters
  , Setter
  , sets
  , mapped

  -- ** Setting Values
  , adjust
  , set
  , (=%=), (=~=), (=+=), (=-=), (=*=), (=/=), (=||=), (=&&=), (=|=), (=&=)

  -- * Manipulating State
  , access
  , (%=), (~=), (+=), (-=), (*=), (//=), (||=), (&&=), (|=), (&=)
  , (%%=)
  , Focus(..)

  -- ** Common Lenses
  , _1
  , _2
  , valueAt
  , valueAtInt
  , bitAt
  , contains
  , containsInt
  , identity
  , resultAt
  , real, imaginary, polarize

  -- * Folds
  , Fold

  -- ** Common Folds
  , folded
  , folding

  -- ** Fold Combinators
  , foldMapOf
  , foldrOf
  , foldOf
  , toListOf
  , anyOf
  , allOf
  , andOf
  , orOf
  , productOf
  , sumOf
  , traverseOf_
  , forOf_
  , sequenceAOf_
  , mapMOf_
  , forMOf_
  , sequenceOf_
  , asumOf
  , msumOf
  , concatMapOf
  , concatOf
  , elemOf
  , notElemOf

  -- * Traversals
  , Traversal

  -- ** Common Traversals
  , traverseNothing

  , traverseValueAt
  , traverseValueAtInt

  , traverseHead, traverseTail
  , traverseLast, traverseInit

  , traverseLeft
  , traverseRight

  , traverseElement
  , traverseElements

  , TraverseByteString(..)
--, TraverseText(..)

  , TraverseValueAtMin(..)
  , TraverseValueAtMax(..)

  , traverseBits
  , traverseDynamic
  , traverseException

  -- ** Traversal Combinators
  -- , traverseOf
  , mapMOf
  , sequenceAOf
  , sequenceOf
  , elementOf
  , elementsOf
  , transposeOf
  ) where

import           Control.Applicative              as Applicative
import           Control.Exception                as Exception
import           Control.Lens.Internal
import           Control.Monad (liftM, MonadPlus(..))
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State.Lazy   as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Reader
import           Data.Bits
import           Data.ByteString.Lazy             as Lazy
import           Data.ByteString                  as Strict
import           Data.Complex
import           Data.Dynamic
import           Data.Foldable                    as Foldable
import           Data.Functor.Identity
import           Data.IntMap                      as IntMap hiding (adjust)
import           Data.IntSet                      as IntSet
import           Data.Map                         as Map    hiding (adjust)
import           Data.Monoid
import           Data.Sequence                    as Seq    hiding (adjust)
import           Data.Set                         as Set
-- import        Data.Text                        as StrictText
-- import        Data.Text.Lazy                   as LazyText
import           Data.Traversable
import           Data.Tree
import           Data.Word (Word8)

infixl 8 ^.
infixr 4 =~=, =%=, =+=, =*=, =-=, =/=, =&&=, =||=, =&=, =|=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=, &=, |=
infixr 0 ^$

--------------------------
-- Lenses
--------------------------

-- | A 'Lens' is actually a lens family as described in <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the lens laws:
--
-- > view l (set l b a)  = b
-- > set l (view l a) a  = a
-- > set l c (set l b a) = set l c a
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot vary fully independently. For more on
-- how they interact, read the "Why is it a Lens Family?" section of <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'Lens' can be used directly as a 'Getter', 'Setter', 'Fold' or 'Traversal'.
--
-- > identity :: Lens (Identity a) (Identity b) a b
-- > identity f (Identity a) = Identity <$> f a
type Lens a b c d = forall f. Functor f => (c -> f d) -> a -> f b

-- | A @'Simple' 'Lens'@, @'Simple' 'Setter'@, or @'Simple' 'Traversal'@ can be used instead of a 'Lens' 'Setter' or 'Traversal' 
-- whenever the type variables don't change upon setting a value.
--
-- > imaginary :: Simple Lens (Complex a) a
-- > imaginary f (e :+ i) = (e :+) <$> f i
--
-- > traverseHead :: Simple Traversal [a] a
type Simple f a b = f a a b b

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- > lens :: Functor f => (a -> c) -> (d -> a -> b) -> (c -> f d) -> a -> f b
lens :: (a -> c) -> (d -> a -> b) -> Lens a b c d
lens ac dab cfd a = (`dab` a) <$> cfd (ac a)
{-# INLINE lens #-}

-- | Built a 'Lens' from an isomorphism family
--
-- > iso :: Functor f => (a -> c) -> (d -> b) -> (c -> f d) -> a -> f b
iso :: (a -> c) -> (d -> b) -> Lens a b c d
iso f g h a = g <$> h (f a )
{-# INLINE iso #-}

---------------
-- Getters
---------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be composed with
-- other lens-like constructions.
--
-- Unlike a 'Lens' a 'Getter' is read-only. Since a 'Getter' cannot be used to write back
-- there are no lens laws that can be applied to it.
--
-- Moreover, a 'Getter' can be used directly as a 'Fold', since it just ignores the 'Monoid'.
--
-- In practice the @b@ and @d@ are left dangling and unused, and as such is no real point in
-- using a @'Simple' 'Getter'@.
type Getter a b c d = forall z. (c -> Const z d) -> a -> Const z b

-- | Build a 'Getter'
--
-- > to f . to g = to (g . f)
to :: (a -> c) -> Getter a b c d
to f g a = Const (getConst (g (f a)))
{-# INLINE to #-}

-------------------------------
-- Getting Values
-------------------------------

-- | View the value pointed to by a 'Getter' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- It may be useful to think of 'view' as having these more restrictive signatures:
--
-- > view ::             Lens a b c d      -> a -> c
-- > view ::             Getter a b c d    -> a -> c
-- > view :: Monoid m => Fold a b m d      -> a -> m
-- > view :: Monoid m => Traversal a b m d -> a -> m
view :: ((c -> Const c d) -> a -> Const c b) -> a -> c
view l a = getConst (l Const a)
{-# INLINE view #-}

-- | View the value of a 'Getter', 'Lens' or the result of folding over the
-- result of mapping the targets of a 'Fold' or 'Traversal'.
--
-- It may be useful to think of 'views' as having these more restrictive signatures:
--
-- > views ::             Getter a b c d    -> (c -> d) -> a -> d
-- > views ::             Lens a b c d      -> (c -> d) -> a -> d
-- > views :: Monoid m => Fold a b c d      -> (c -> m) -> a -> m
-- > views :: Monoid m => Traversal a b c d -> (c -> m) -> a -> m
views :: ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
views l f = getConst . l (Const . f)
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view', only infix.
--
-- > (^$) ::             Lens a b c d      -> a -> c
-- > (^$) ::             Getter a b c d    -> a -> c
-- > (^$) :: Monoid m => Fold a b m d      -> a -> m
-- > (^$) :: Monoid m => Traversal a b m d -> a -> m
(^$) :: ((c -> Const c d) -> a -> Const c b) -> a -> c
l ^$ a = getConst (l Const a)
{-# INLINE (^$) #-}

-- | View the value pointed to by a 'Getter' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with (Prelude..)
--
-- > ghci> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- > 2.23606797749979
--
-- > (^$) ::             a -> Lens a b c d      -> c
-- > (^$) ::             a -> Getter a b c d    -> c
-- > (^$) :: Monoid m => a -> Fold a b m d      -> m
-- > (^$) :: Monoid m => a -> Traversal a b m d -> m
(^.) :: a -> ((c -> Const c d) -> a -> Const c b) -> c
a ^. l = getConst (l Const a)
{-# INLINE (^.) #-}

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- The only 'Lens'-like law that applies to a 'Setter' @l@ is that
--
-- > set l c (set l b a) = set l c a
--
-- You can't 'view' a 'Setter' in general, so the other two laws do not apply.
--
-- You can compose a 'Setter' with a 'Lens' or a 'Traversal' using @(.)@ from the Prelude
-- and the result is always only a 'Setter' and nothing more.
type Setter a b c d = (c -> Identity d) -> a -> Identity b

-- | This setter can be used to map over all of the values in a container.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- | Build a Setter
--
-- > sets . adjust = id
-- > adjust . sets = id
sets :: ((c -> d) -> a -> b) -> Setter a b c d
sets f g a = Identity (f (runIdentity . g) a)
{-# INLINE sets #-}

-- | Modify the target of a 'Lens' or all the targets of a 'Setter' or 'Traversal'
-- with a function.
--
-- > fmap = adjust traverse
--
-- Two useful free theorems hold for this type:
--
-- > sets . adjust = id
-- > adjust . sets = id
--
-- > adjust :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
adjust :: Setter a b c d -> (c -> d) -> a -> b
adjust l f a = runIdentity (l (Identity . f) a)
{-# INLINE adjust #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter'
-- or 'Traversal' with a constant value.
--
-- > (<$) = set traverse
--
-- > set :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
set :: Setter a b c d -> d -> a -> b
set l d a = runIdentity (l (\_ -> Identity d) a)
{-# INLINE set #-}

-- | Modifies the target of a 'Lens' or all of the targets of a 'Setter' or
-- 'Traversal' with a user supplied function.
--
-- This is an infix version of 'adjust'
--
-- > fmap f = traverse =%= f
--
-- > (=%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
(=%=) :: Setter a b c d -> (c -> d) -> a -> b
l =%= f = runIdentity . l (Identity . f)
{-# INLINE (=%=) #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter'
-- or 'Traversal' with a constant value.
--
-- This is an infix version of 'set'
--
-- > f <$ a = traverse =~= f $ a
--
-- > (=~=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
(=~=) :: Setter a b c d -> d -> a -> b
l =~= v = runIdentity . l (Identity . const v)
{-# INLINE (=~=) #-}

-- | Increment the target(s) of a numerically valued 'Lens', Setter' or 'Traversal'
--
-- > ghci> _1 =+= 1 $ (1,2)
-- > (2,2)
--
-- > (=+=) :: Num c => ((c -> Identity c) -> a -> Identity b) -> c -> a -> b
(=+=) :: Num c => Setter a b c c -> c -> a -> b
l =+= n = adjust l (+ n)
{-# INLINE (=+=) #-}

-- | Multiply the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal'
--
-- > ghci> _2 =*= 4 $ (1,2)
-- > (1,8)
--
-- > (=*=) :: Num c => ((c -> Identity c) -> a -> Identity b) -> c -> a -> b
(=*=) :: Num c => Setter a b c c -> c -> a -> b
l =*= n = adjust l (* n)
{-# INLINE (=*=) #-}

-- | Decrement the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal'
--
-- > ghci> _1 =-= 2 $ (1,2)
-- > (-1,2)
--
-- > (=-=) :: ((c -> Identity c) -> a -> Identity b) -> c -> a -> b
(=-=) :: Num c => Setter a b c c -> c -> a -> b
l =-= n = adjust l (subtract n)
{-# INLINE (=-=) #-}

-- | Divide the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal'
--
-- > (=/=) :: Fractional c => ((c -> Identity c) -> a -> Identity b) -> c -> a -> b
(=/=) :: Fractional c => Setter a b c c -> c -> a -> b
l =/= n = adjust l (/ n)

-- | Logically '||' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- > (=||=):: ((Bool -> Identity Bool) -> a -> Identity b) -> Bool -> a -> b
(=||=):: Setter a b Bool Bool -> Bool -> a -> b
l =||= n = adjust l (|| n)
{-# INLINE (=||=) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- (=&&=) :: ((Bool -> Identity Bool) -> a -> Identity b) -> Bool -> a -> b
(=&&=) :: Setter a b Bool Bool -> Bool -> a -> b
l =&&= n = adjust l (&& n)
{-# INLINE (=&&=) #-}

-- | Bitwise '.|.' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- > (=|=):: Bits c => ((c -> Identity c) -> a -> Identity b) -> Bool -> a -> b
(=|=):: Bits c => Setter a b c c -> c -> a -> b
l =|= n = adjust l (.|. n)
{-# INLINE (=|=) #-}

-- | Bitwise '.&.' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- > (=&=) :: Bits c => ((b -> Identity b) -> a -> Identity a) -> c -> a -> b
(=&=) :: Bits c => Setter a b c c -> c -> a -> b
l =&= n = adjust l (.&. n)
{-# INLINE (=&=) #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | This is a lens that can change the value (and type) of the first field of
-- a pair.
--
-- > ghci> (1,2)^._1
-- > 1
--
-- > ghci> _1 =+= "hello" $ (1,2)
-- > ("hello",2)
--
-- > _1 :: Functor f => (a -> f b) -> (a,c) -> f (a,c)
_1 :: Lens (a,c) (b,c) a b
_1 f (a,c) = (\b -> (b,c)) <$> f a
{-# INLINE _1 #-}

-- | As '_1', but for the second field of a pair.
--
-- > anyOf _2 :: (c -> Bool) -> (a, c) -> Bool
-- > traverse._2 :: (Applicative f, Traversable t) => (a -> f b) -> t (c, a) -> f (t (c, b))
-- > foldMapOf (traverse._2) :: (Traversable t, Monoid m) => (c -> m) -> t (b, c) -> m
--
-- > _2 :: Functor f => (a -> f b) -> (c,a) -> f (c,b)
_2 :: Lens (c,a) (c,b) a b
_2 f (c,a) = (,) c <$> f a
{-# INLINE _2 #-}

-- | This 'Lens' can be used to read, write or delete the value associated with a key in a 'Map'.
--
-- > ghci> Map.fromList [("hello",12)] ^. valueAt "hello"
-- > Just 12
--
-- > valueAt :: Ord k => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
valueAt :: Ord k => k -> Simple Lens (Map k v) (Maybe v)
valueAt k f m = go <$> f (Map.lookup k m) where
  go Nothing   = Map.delete k m
  go (Just v') = Map.insert k v' m
{-# INLINE valueAt #-}

-- | This 'Lens' can be used to read, write or delete a member of an 'IntMap'.
--
-- > ghci> IntMap.fromList [(1,"hello")]  ^. valueAtInt 1
-- > Just "hello"
--
-- > ghci> valueAtInt 2 =+= "goodbye" $ IntMap.fromList [(1,"hello")]
-- > fromList [(1,"hello"),(2,"goodbye")]
--
-- > valueAtInt :: Int -> (Maybe v -> f (Maybe v)) -> IntMap v -> f (IntMap v)
valueAtInt :: Int -> Simple Lens (IntMap v) (Maybe v)
valueAtInt k f m = go <$> f (IntMap.lookup k m) where
  go Nothing   = IntMap.delete k m
  go (Just v') = IntMap.insert k v' m
{-# INLINE valueAtInt #-}

-- | This 'Lens' can be used to read, write or delete a member of a 'Set'
--
-- > ghci> contains 3 =+= False $ Set.fromList [1,2,3,4]
-- > fromList [1,2,4]
--
-- > contains :: Ord k => k -> (Bool -> f Bool) -> Set k -> f (Set k)
contains :: Ord k => k -> Simple Lens (Set k) Bool
contains k f s = go <$> f (Set.member k s) where
  go False = Set.delete k s
  go True  = Set.insert k s
{-# INLINE contains #-}

-- | This 'Lens' can be used to read, write or delete a member of an 'IntSet'
--
-- > ghci> containsInt 3 =+= False $ IntSet.fromList [1,2,3,4]
-- > fromList [1,2,4]
--
-- > containsInt :: Int -> (Bool -> f Bool) -> IntSet -> f IntSet
containsInt :: Int -> Simple Lens IntSet Bool
containsInt k f s = go <$> f (IntSet.member k s) where
  go False = IntSet.delete k s
  go True  = IntSet.insert k s
{-# INLINE containsInt #-}

-- | This lens can be used to access the contents of the Identity monad
identity :: Lens (Identity a) (Identity b) a b
identity f (Identity a) = Identity <$> f a
{-# INLINE identity #-}

-- | This lens can be used to access the value of the nth bit in a number.
--
-- @bitsAt n@ is only a legal 'Lens' into @b@ if @0 <= n < bitSize (undefined :: b)@

bitAt :: Bits b => Int -> Simple Lens b Bool
bitAt n f b = (\x -> if x then setBit b n else clearBit b n) <$> f (testBit b n)
{-# INLINE bitAt #-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
resultAt :: Eq e => e -> Simple Lens (e -> a) a
resultAt e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE resultAt #-}

-- | Access the real part of a complex number
--
-- > real :: Functor f => (a -> f a) -> Complex a -> f (Complex a)
real :: Simple Lens (Complex a) a
real f (a :+ b) = (:+ b) <$> f a

-- | Access the imaginary part of a complex number
--
-- > imaginary :: Functor f => (a -> f a) -> Complex a -> f (Complex a)
imaginary :: Simple Lens (Complex a) a
imaginary f (a :+ b) = (a :+) <$> f b

-- | This isn't /quite/ a legal lens. Notably the @view l (set l b a) = b@ law
-- is violated when you set a polar value with 0 magnitude and non-zero phase
-- as the phase information is lost.
--
-- So don't do that. Otherwise this is a perfectly convenient lens.
--
-- polarize :: Functor f => ((a,a) -> f (a,a)) -> Complex a -> f (Complex a)
polarize :: RealFloat a => Simple Lens (Complex a) (a,a)
polarize f c = uncurry mkPolar <$> f (polar c)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

-- |
-- Access the target of a 'Lens' or 'Getter' in the current state, or access a
-- summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- > access :: MonadState a m             => Getter a b c d    -> m c
-- > access :: MonadState a m             => Lens a b c d      -> m c
-- > access :: (MonadState a m, Monoid c) => Fold a b c d      -> m c
-- > access :: (MonadState a m, Monoid c) => Traversal a b c d -> m c
access :: MonadState a m => ((c -> Const c d) -> a -> Const c b) -> m c
access l = gets (^. l)
{-# INLINE access #-}

-- | This class allows us to use 'focus' on a number of different monad transformers.
class Focus st where
  -- | Run a monadic action in a larger context than it was defined in, using a 'Simple' 'Lens' or 'Simple Traversal'.
  --
  -- This is commonly used to lift actions in a simpler state monad into a state monad with a larger state type.
  --
  -- When applied to a 'Simple 'Traversal' over multiple values, the actions for each target are executed sequentially
  -- and the results are aggregated monoidally
  -- and a monoidal summary
  -- of the result is given.
  --
  -- > focus :: Monad m => Simple Lens a b -> st b m c -> st a m c
  -- > focus :: (Monad m, Monoid c) => Simple Traversal a b -> st b m c -> st a m c
  focus :: Monad m => ((b -> Focusing m c b) -> a -> Focusing m c a) -> st b m c -> st a m c

  -- | 'focus', discarding any accumulated results as you go.
  focus_ :: Monad m => ((b -> Focusing m () b) -> a -> Focusing m () a) -> st b m c -> st a m ()

skip :: a -> ()
skip _ = ()

instance Focus Strict.StateT where
  focus l m = Strict.StateT $ \a -> unfocusing (l (Focusing . Strict.runStateT m) a)
  {-# INLINE focus #-}
  focus_ l m = Strict.StateT $ \a -> unfocusing (l (Focusing . Strict.runStateT (liftM skip m)) a)
  {-# INLINE focus_ #-}

instance Focus Lazy.StateT where
  focus l m = Lazy.StateT $ \a -> unfocusing (l (Focusing . Lazy.runStateT m) a)
  {-# INLINE focus #-}
  focus_ l m = Lazy.StateT $ \a -> unfocusing (l (Focusing . Lazy.runStateT (liftM skip m)) a)
  {-# INLINE focus_ #-}

-- | We can focus Reader environments, too!
instance Focus ReaderT where
  focus l m = ReaderT $ \a -> liftM undefined $  unfocusing $ l (\b -> Focusing $ (\c -> (c,b)) `liftM` runReaderT m b) a
  {-# INLINE focus #-}
  focus_ l m = ReaderT $ \a -> liftM undefined $  unfocusing $ l (\b -> Focusing $ (\_ -> ((),b)) `liftM` runReaderT m b) a
  {-# INLINE focus_ #-}

-- | Modify the target of a 'Lens' in the current state returning some extra information of @c@ or
-- modify all targets of a 'Traversal' in the current state, extracting extra information of type @c@
-- and return a monoidal summary of the changes.
--
-- It may be useful to think of '(%%=)', instead, as having either of the following more restricted
-- type signatures:
--
-- > (%%=) :: MonadState a m => Simple Lens a b -> (b -> (c, b) -> m c
-- > (%%=) :: (MonadState a m, Monoid c) => Simple Traversal a b -> (b -> (c, b) -> m c
(%%=) :: MonadState a m => ((b -> (c,b)) -> a -> (c,a)) -> (b -> (c, b)) -> m c
l %%= f = state (l f)
{-# INLINE (%%=) #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal' in our monadic
-- state with a new value, irrespective of the old.
(~=) :: MonadState a m => Setter a a c d -> d -> m ()
l ~= b = modify $ l =~= b
{-# INLINE (~=) #-}

-- | Map over the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal in our monadic state.
(%=) :: MonadState a m => Setter a a c d -> (c -> d) -> m ()
l %= f = modify $ l =%= f
{-# INLINE (%=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by adding a value
(+=) :: (MonadState a m, Num b) => Simple Setter a b -> b -> m ()
l += b = modify $ l =+= b
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by subtracting a value
(-=) :: (MonadState a m, Num b) => Simple Setter a b -> b -> m ()
l -= b = modify $ l =-= b
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by multiplying by value
(*=) :: (MonadState a m, Num b) => Simple Setter a b -> b -> m ()
l *= b = modify $ l =*= b
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by dividing by a value
(//=) ::  (MonadState a m, Fractional b) => Simple Setter a b -> b -> m ()
l //= b = modify $ l =/= b
{-# INLINE (//=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by taking their logical '&&' with a value
(&&=):: MonadState a m => Simple Setter a Bool -> Bool -> m ()
l &&= b = modify $ l =&&= b
{-# INLINE (&&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by taking their logical '||' with a value
(||=) :: MonadState a m => Simple Setter a Bool -> Bool -> m ()
l ||= b = modify $ l =||= b
{-# INLINE (||=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.&.' with another value.
(&=):: (MonadState a m, Bits b) => Simple Setter a b -> b -> m ()
l &= b = modify $ l =&= b
{-# INLINE (&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
(|=) :: (MonadState a m, Bits b) => Simple Setter a b -> b -> m ()
l |= b = modify $ l =|= b
{-# INLINE (|=) #-}

--------------------------
-- Folds
--------------------------
-- | A 'Fold' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A @'Fold' a b c d@ provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f c)@, then there should be a
-- 'fooOf' method that takes a @'Fold' a b c d@ and a value of type @a@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Monoid'
--
-- Unlike a 'Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no lens laws that can be applied to it.
--
-- In practice the @b@ and @d@ are left dangling and unused, and as such is no real point in a @'Simple' 'Fold'@.
type Fold a b c d      = forall m. Monoid m => (c -> Const m d) -> a -> Const m b

-- | Building a Fold
folding :: Foldable f => (a -> f c) -> Fold a b c d
folding f g a = Const (foldMap (getConst . g) (f a))
{-# INLINE folding #-}

-- | Obtain a 'Fold' from any 'Foldable'
folded :: Foldable f => Fold (f c) b c d
folded = folding id
{-# INLINE folded #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- |
-- > foldMap = foldMapOf folded
--
-- > foldMapOf = views
--
-- > foldMapOf ::             Getter a b c d    -> (c -> m) -> a -> m
-- > foldMapOf ::             Lens a b c d      -> (c -> m) -> a -> m
-- > foldMapOf :: Monoid m => Fold a b c d      -> (c -> m) -> a -> m
-- > foldMapOf :: Monoid m => Traversal a b c d -> (c -> m) -> a -> m
foldMapOf :: ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

-- |
-- > fold = foldOf folded
--
-- > foldOf = view
--
-- > foldOf ::             Getter a b m d    -> a -> m
-- > foldOf ::             Lens a b m d      -> a -> m
-- > foldOf :: Monoid m => Fold a b m d      -> a -> m
-- > foldOf :: Monoid m => Traversal a b m d -> a -> m
foldOf :: ((m -> Const m d) -> a -> Const m b) -> a -> m
foldOf l = getConst . l Const
{-# INLINE foldOf #-}

-- |
-- > foldr = foldrOf folded
--
-- > foldrOf :: Getter a b c d    -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Lens a b c d      -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Fold a b c d      -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: Traversal a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf :: ((c -> Const (Endo e) d) -> a -> Const (Endo e) b) -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- |
-- > toList = toListOf folded
--
-- > toListOf :: Getter a b c d    -> a -> [c]
-- > toListOf :: Lens a b c d      -> a -> [c]
-- > toListOf :: Fold a b c d      -> a -> [c]
-- > toListOf :: Traversal a b c d -> a -> [c]
toListOf :: ((c -> Const [c] d) -> a -> Const [c] b) -> a -> [c]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- |
-- > and = andOf folded
--
-- > andOf :: Getter a b Bool d   -> a -> Bool
-- > andOf :: Lens a b Bool d     -> a -> Bool
-- > andOf :: Fold a b Bool d     -> a -> Bool
-- > andOf :: Traversl a b Bool d -> a -> Bool
andOf :: ((Bool -> Const All d) -> a -> Const All b) -> a -> Bool
andOf l = getAll . foldMapOf l All
{-# INLINE andOf #-}

-- |
-- > or = orOf folded
--
-- > orOf :: Getter a b Bool d    -> a -> Bool
-- > orOf :: Lens a b Bool d      -> a -> Bool
-- > orOf :: Fold a b Bool d      -> a -> Bool
-- > orOf :: Traversal a b Bool d -> a -> Bool
orOf :: ((Bool -> Const Any d) -> a -> Const Any b) -> a -> Bool
orOf l = getAny . foldMapOf l Any
{-# INLINE orOf #-}

-- |
-- > any = anyOf folded
--
-- > anyOf :: Getter a b c d    -> (c -> Bool) -> a -> Bool
-- > anyOf :: Lens a b c d      -> (c -> Bool) -> a -> Bool
-- > anyOf :: Fold a b c d      -> (c -> Bool) -> a -> Bool
-- > anyOf :: Traversal a b c d -> (c -> Bool) -> a -> Bool
anyOf :: ((c -> Const Any d) -> a -> Const Any b) -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
{-# INLINE anyOf #-}

-- |
-- > all = allOf folded
--
-- > allOf :: Getter a b c d    -> (c -> Bool) -> a -> Bool
-- > allOf :: Lens a b c d      -> (c -> Bool) -> a -> Bool
-- > allOf :: Fold a b c d      -> (c -> Bool) -> a -> Bool
-- > allOf :: Traversal a b c d -> (c -> Bool) -> a -> Bool
allOf :: ((c -> Const All d) -> a -> Const All b) -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
{-# INLINE allOf #-}

-- |
-- > product = productOf folded
--
-- > productOf ::          Getter a b c d    -> a -> c
-- > productOf ::          Lens a b c d      -> a -> c
-- > productOf :: Num c => Fold a b c d      -> a -> c
-- > productOf :: Num c => Traversal a b c d -> a -> c
productOf :: ((c -> Const (Product c) d) -> a -> Const (Product c) b) -> a -> c
productOf l = getProduct . foldMapOf l Product
{-# INLINE productOf #-}

-- |
-- > sum = sumOf folded
--
-- > sumOf _1 :: (a, b) -> a
-- > sumOf (folded._1) :: (Foldable f, Num a) => f (a, b) -> a
--
-- > sumOf ::          Getter a b c d    -> a -> c
-- > sumOf ::          Lens a b c d      -> a -> c
-- > sumOf :: Num c => Fold a b c d      -> a -> c
-- > sumOf :: Num c => Traversal a b c d -> a -> c
sumOf ::  ((c -> Const (Sum c) d) -> a -> Const (Sum c) b) -> a -> c
sumOf l = getSum . foldMapOf l Sum
{-# INLINE sumOf #-}

-- |
--
-- When passed a 'Getter', 'traverseOf_' can work over a 'Functor'.
--
-- When passed a 'Fold', 'traverseOf_' requires an 'Applicative'.
--
-- > traverse_ = traverseOf_ folded
--
-- > traverseOf_ _2 :: Functor f => (c -> f e) -> (c1, c) -> f ()
-- > traverseOf_ traverseLeft :: Applicative f => (a -> f b) -> Either a c -> f ()
--
-- The rather specific signature of traverseOf_ allows it to be used as if the signature was either:
--
-- > traverseOf_ :: Functor f     => Getter a b c d    -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Functor f     => Lens a b c d      -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Applicative f => Fold a b c d      -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Applicative f => Traversal a b c d -> (c -> f e) -> a -> f ()
traverseOf_ :: Functor f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversed . foldMapOf l (Traversed . (() <$) . f)
{-# INLINE traverseOf_ #-}

-- |
-- > for_ = forOf_ folded
--
-- > forOf_ :: Functor f     => Getter a b c d    -> a -> (c -> f e) -> f ()
-- > forOf_ :: Functor f     => Lens a b c d      -> a -> (c -> f e) -> f ()
-- > forOf_ :: Applicative f => Fold a b c d      -> a -> (c -> f e) -> f ()
-- > forOf_ :: Applicative f => Traversal a b c d -> a -> (c -> f e) -> f ()
forOf_ :: Functor f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> (c -> f e) -> f ()
forOf_ l a f = traverseOf_ l f a
{-# INLINE forOf_ #-}

-- |
-- > sequenceA_ = sequenceAOf_ folded
--
-- > sequenceAOf_ :: Functor f     => Getter a b (f ()) d    -> a -> f ()
-- > sequenceAOf_ :: Functor f     => Lens a b (f ()) d      -> a -> f ()
-- > sequenceAOf_ :: Applicative f => Fold a b (f ()) d      -> a -> f ()
-- > sequenceAOf_ :: Applicative f => Traversal a b (f ()) d -> a -> f ()
sequenceAOf_ :: Functor f => ((f () -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> f ()
sequenceAOf_ l = getTraversed . foldMapOf l (Traversed . (() <$))
{-# INLINE sequenceAOf_ #-}

-- |
-- > mapM_ = mapMOf_ folded
--
-- > mapMOf_ :: Monad m => Getter a b c d    -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Lens a b c d      -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Fold a b c d      -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => Traversal a b c d -> (c -> m e) -> a -> m ()
mapMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> (c -> m e) -> a -> m ()
mapMOf_ l f = unwrapMonad . traverseOf_ l (WrapMonad . f)
{-# INLINE mapMOf_ #-}

-- |
-- > forM_ = forMOf_ folded
--
-- > forMOf_ :: Monad m => Getter a b c d    -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Lens a b c d      -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Fold a b c d      -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => Traversal a b c d -> a -> (c -> m e) -> m ()
forMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> (c -> m e) -> m ()
forMOf_ l a f = mapMOf_ l f a
{-# INLINE forMOf_ #-}

-- |
-- > sequence_ = sequenceOf_ folded
--
-- > sequenceOf_ :: Monad m => Getter a b (m b) d    -> a -> m ()
-- > sequenceOf_ :: Monad m => Lens a b (m b) d      -> a -> m ()
-- > sequenceOf_ :: Monad m => Fold a b (m b) d      -> a -> m ()
-- > sequenceOf_ :: Monad m => Traversal a b (m b) d -> a -> m ()
sequenceOf_ :: Monad m => ((m c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> m ()
sequenceOf_ l = unwrapMonad . traverseOf_ l WrapMonad
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > asum = asumOf folded
--
-- > asumOf :: Alternative f => Getter a b c d    -> a -> f c
-- > asumOf :: Alternative f => Lens a b c d      -> a -> f c
-- > asumOf :: Alternative f => Fold a b c d      -> a -> f c
-- > asumOf :: Alternative f => Traversal a b c d -> a -> f c
asumOf :: Alternative f => ((f c -> Const (Endo (f c)) d) -> a -> Const (Endo (f c)) b) -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > msum = msumOf folded
--
-- > msumOf :: MonadPlus m => Getter a b c d    -> a -> m c
-- > msumOf :: MonadPlus m => Lens a b c d      -> a -> m c
-- > msumOf :: MonadPlus m => Fold a b c d      -> a -> m c
-- > msumOf :: MonadPlus m => Traversal a b c d -> a -> m c
msumOf :: MonadPlus m => ((m c -> Const (Endo (m c)) d) -> a -> Const (Endo (m c)) b) -> a -> m c
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- |
-- > elem = elemOf folded
--
-- > elemOf :: Eq c => Getter a b c d    -> c -> a -> Bool
-- > elemOf :: Eq c => Lens a b c d      -> c -> a -> Bool
-- > elemOf :: Eq c => Fold a b c d      -> c -> a -> Bool
-- > elemOf :: Eq c => Traversal a b c d -> c -> a -> Bool
elemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- |
-- > notElem = notElemOf folded
--
-- > notElemOf :: Eq c => Getter a b c d    -> c -> a -> Bool
-- > notElemOf :: Eq c => Fold a b c d      -> c -> a -> Bool
-- > notElemOf :: Eq c => Lens a b c d      -> c -> a -> Bool
-- > notElemOf :: Eq c => Traversal a b c d -> c -> a -> Bool
notElemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
notElemOf l c = not . elemOf l c
{-# INLINE notElemOf #-}

-- |
-- > concatMap = concatMapOf folded
--
-- > concatMapOf :: Getter a b c d     -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Lens a b c d      -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Fold a b c d      -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: Traversal a b c d -> (c -> [e]) -> a -> [e]
concatMapOf :: ((c -> Const [e] d) -> a -> Const [e] b) -> (c -> [e]) -> a -> [e]
concatMapOf l ces a = getConst  (l (Const . ces) a)
{-# INLINE concatMapOf #-}

-- |
-- > concat = concatOf folded
--
-- > concatOf :: Getter a b [e] d -> a -> [e]
-- > concatOf :: Lens a b [e] d -> a -> [e]
-- > concatOf :: Fold a b [e] d -> a -> [e]
-- > concatOf :: a b [e] d -> a -> [e]
concatOf :: (([e] -> Const [e] d) -> a -> Const [e] b) -> a -> [e]
concatOf = view
{-# INLINE concatOf #-}

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' can be used directly as a 'Setter' or a 'Fold' (but not as a 'Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'Traversal' laws.
--
-- These are also known as @MultiLens@ families, but they have the signature and spirit of
--
-- > traverse :: Traversable f => Traversal (f a) (f b) a b
--
-- and the more evocative name suggests their application.
type Traversal a b c d        = forall f. Applicative f => (c -> f d) -> a -> f b

--------------------------
-- Traversal combinators
--------------------------

-- |
-- > mapM = mapMOf traverse
--
-- > mapMOf :: Monad m => Lens a b c d      -> (c -> m d) -> a -> m b
-- > mapMOf :: Monad m => Traversal a b c d -> (c -> m d) -> a -> m b
mapMOf :: ((c -> WrappedMonad m d) -> a -> WrappedMonad m b) -> (c -> m d) -> a -> m b
mapMOf l cmd a = unwrapMonad (l (WrapMonad . cmd) a)
{-# INLINE mapMOf #-}

-- |
-- > sequenceA = sequenceAOf traverse
--
-- > sequenceAOf :: Applicative f => Lens a b (f c) (f c)      -> a -> f b
-- > sequenceAOf :: Applicative f => Traversal a b (f c) (f c) -> a -> f b
sequenceAOf :: Applicative f => ((f c -> f (f c)) -> a -> f b) -> a -> f b
sequenceAOf l = l pure
{-# INLINE sequenceAOf #-}

-- |
-- > sequence = sequenceOf traverse
--
-- > sequenceOf :: Monad m => Lens a b (m c) (m c)      -> a -> m b
-- > sequenceOf :: Monad m => Traversal a b (m c) (m c) -> a -> m b
sequenceOf :: Monad m => ((m c -> WrappedMonad m (m c)) -> a -> WrappedMonad m b) -> a -> m b
sequenceOf l = unwrapMonad . l pure
{-# INLINE sequenceOf #-}

-- | A 'Traversal' of the nth element of another 'Traversal'
--
-- > traverseHead = elementOf traverse 0
elementOf :: Applicative f => ((c -> AppliedState f c) -> a -> AppliedState f b) -> Int -> (c -> f c) -> a -> f b
elementOf l = elementsOf l . (==)

-- | A 'Traversal' of the elements in another 'Traversal' where their positions in that 'Traversal' satisfy a predicate
--
-- > traverseTail = elementsOf traverse (>0)
elementsOf :: Applicative f => ((c -> AppliedState f c) -> a -> AppliedState f b) -> (Int -> Bool) -> (c -> f c) -> a -> f b
elementsOf l p f ta = fst (runAppliedState (l go ta) 0) where
  go a = AppliedState $ \i -> (if p i then f a else pure a, i + 1)

-- |
-- > transpose = transposeOf traverse -- modulo the ragged arrays support
--
-- > transposeOf _2 :: (b, [a]) -> [(b, a)]

transposeOf :: (([c] -> ZipList c) -> a -> ZipList b) -> a -> [b]
transposeOf l = getZipList . l ZipList

--------------------------
-- Traversals
--------------------------

-- | This is the traversal that never succeeds at returning any values
--
-- > traverseNothing :: Applicative f => (c -> f d) -> a -> f a
traverseNothing :: Traversal a a c d
traverseNothing = const pure
{-# INLINE traverseNothing #-}

-- The traversal for reading and writing to the head of a list
--
-- > traverseHead = traverseValueAtMin
-- > traverseHead = traverseElementAt 0 -- but is more efficient
--
-- | > traverseHead :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseHead :: Simple Traversal [a] a
traverseHead _ [] = pure []
traverseHead f (a:as) = (:as) <$> f a
{-# INLINE traverseHead #-}

-- | > traverseTail :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
traverseTail :: Simple Traversal [a] [a]
traverseTail _ [] = pure []
traverseTail f (a:as) = (a:) <$> f as
{-# INLINE traverseTail #-}

-- | Traverse the last element in a list.
--
-- > traverseLast = traverseValueAtMax
--
-- > traverseLast :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseLast :: Simple Traversal [a] a
traverseLast _ []     = pure []
traverseLast f [a]    = return <$> f a
traverseLast f (a:as) = (a:) <$> traverseLast f as
{-# INLINE traverseLast #-}

-- The traversal for reading and writing to the tail of a list

-- | Traverse all but the last element of a list
--
-- > traverseInit :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
traverseInit :: Simple Traversal [a] [a]
traverseInit _ [] = pure []
traverseInit f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE traverseInit #-}

-- | A traversal for tweaking the left-hand value in an Either:
--
-- > traverseLeft :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
traverseLeft :: Traversal (Either a c) (Either b c) a b
traverseLeft f (Left a)  = Left <$> f a
traverseLeft _ (Right c) = pure $ Right c
{-# INLINE traverseLeft #-}

-- | traverse the right-hand value in an Either:
--
-- > traverseRight :: Applicative f => (a -> f b) -> Either c a -> f (Either c a)
-- > traverseRight = traverse
--
-- Unfortunately the instance for 'Traversable (Either c)' is still missing from
-- base, so this can't just be 'traverse'
traverseRight :: Traversal (Either c a) (Either c b) a b
traverseRight _ (Left c) = pure $ Left c
traverseRight f (Right a) = Right <$> f a
{-# INLINE traverseRight #-}

-- | Traverse the value at a given key in a Map
--
-- > traverseValueAt :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > traverseValueAt k = valueAt k . traverse
traverseValueAt :: Ord k => k -> Simple Traversal (Map k v) v
traverseValueAt k = valueAt k . traverse
{-# INLINE traverseValueAt #-}

-- | Traverse the value at a given key in an IntMap
--
-- > traverseValueAtInt :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > traverseValueAtInt k = valueAtInt k . traverse
traverseValueAtInt :: Int -> Simple Traversal (IntMap v) v
traverseValueAtInt k = valueAtInt k . traverse
{-# INLINE traverseValueAtInt #-}

-- | Traverse a single element in a traversable container.
--
-- > traverseElement :: (Applicative f, Traversable t) => Int -> (a -> f a) -> t a -> f (t a)
traverseElement :: Traversable t => Int -> Simple Traversal (t a) a
traverseElement = traverseElements . (==)
{-# INLINE traverseElement #-}

-- | Traverse elements where a predicate holds on their position in a traversable container
--
-- > traverseElements :: Applicative f, Traversable t) => (Int -> Bool) -> (a -> f a) -> t a -> f (t a)
traverseElements :: Traversable t => (Int -> Bool) -> Simple Traversal (t a) a
traverseElements p f ta = fst (runAppliedState (traverse go ta) 0) where
  go a = AppliedState $ \i -> (if p i then f a else pure a, i + 1)
{-# INLINE traverseElements #-}

-- |
-- Traverse the typed value contained in a 'Dynamic' where the type required by your function matches that
-- of the contents of the 'Dynamic'.
--
-- > traverseDynamic :: (Applicative f, Typeable a, Typeable b) => (a -> f b) -> Dynamic -> f Dynamic
traverseDynamic :: (Typeable a, Typeable b) => Traversal Dynamic Dynamic a b
traverseDynamic f dyn = case fromDynamic dyn of
  Just a  -> toDyn <$> f a
  Nothing -> pure dyn

-- |
-- Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- > traverseException :: (Applicative f, Exception a, Exception b) => (a -> f b) -> SomeException -> f SomeException
traverseException :: (Exception a, Exception b) => Traversal SomeException SomeException a b
traverseException f e = case fromException e of
  Just a -> toException <$> f a
  Nothing -> pure e

-- | Provides ad hoc overloading for 'traverseByteString'
class TraverseByteString t where
  -- | Traverse the individual bytes in a ByteString
  --
  -- > anyOf traverseByteString (==0x80) :: TraverseByteString b => b -> Bool
  traverseByteString :: Simple Traversal t Word8

instance TraverseByteString Strict.ByteString where
  traverseByteString f = fmap Strict.pack . traverse f . Strict.unpack

instance TraverseByteString Lazy.ByteString where
  traverseByteString f = fmap Lazy.pack . traverse f . Lazy.unpack

{-
-- | Provides ad hoc overloading for 'traverseText'
class TraverseText t where
  traverseText :: Simple Traversal t Char

instance TraverseText StrictText.Text where
  traverseText f = fmap StrictText.pack . traverse f . StrictText.unpack

instance TraverseText LazyText.Text where
  traverseText f = fmap LazyText.pack . traverse f . LazyText.unpack
-}

-- | Types that support traversal of the value of the minimal key
--
-- This is separate from 'TraverseValueAtMax' because a min-heap
-- or max-heap may be able to support one, but not the other.
class TraverseValueAtMin t where
  -- | Traverse the value for the minimal key
  traverseValueAtMin :: Simple Traversal (t v) v
  -- default traverseValueAtMin :: Traversable t => Traversal (t v) v
  -- traverseValueAtMin = traverseElement 0

instance TraverseValueAtMin (Map k) where
  traverseValueAtMin f m = case Map.minView m of
    Just (a, _) -> (\v -> Map.updateMin (const (Just v)) m) <$> f a
    Nothing     -> pure m

instance TraverseValueAtMin IntMap where
  traverseValueAtMin f m = case IntMap.minView m of
    Just (a, _) -> (\v -> IntMap.updateMin (const v) m) <$> f a
    Nothing     -> pure m

instance TraverseValueAtMin [] where
  traverseValueAtMin = traverseHead

instance TraverseValueAtMin Seq where
  traverseValueAtMin f m = case Seq.viewl m of
    a :< as -> (<| as) <$> f a
    EmptyL -> pure m

instance TraverseValueAtMin Tree where
  traverseValueAtMin f (Node a as) = (`Node` as) <$> f a

-- | Types that support traversal of the value of the maximal key
--
-- This is separate from 'TraverseValueAtMin' because a min-heap
-- or max-heap may be able to support one, but not the other.
class TraverseValueAtMax t where
  -- | Traverse the value for the maximal key
  traverseValueAtMax :: Simple Traversal (t v) v

instance TraverseValueAtMax (Map k) where
  traverseValueAtMax f m = case Map.maxView m of
    Just (a, _) -> (\v -> Map.updateMax (const (Just v)) m) <$> f a
    Nothing     -> pure m

instance TraverseValueAtMax IntMap where
  traverseValueAtMax f m = case IntMap.maxView m of
    Just (a, _) -> (\v -> IntMap.updateMax (const v) m) <$> f a
    Nothing     -> pure m

instance TraverseValueAtMax [] where
  traverseValueAtMax = traverseLast

instance TraverseValueAtMax Seq where
  traverseValueAtMax f m = case Seq.viewr m of
    as :> a -> (as |>) <$> f a
    EmptyR  -> pure m

-- | Traverse over all bits in a numeric type.
--
-- > ghci> toListOf traverseBits (5 :: Word8)
-- > [True,False,True,False,False,False,False,False]
--
-- If you supply this an Integer, it won't crash, but the result will
-- be an infinite traversal that can be productively consumed.
--
-- > ghci> toListOf traverseBits 5
-- > [True,False,True,False,False,False,False,False,False,False,False,False...

traverseBits :: Bits b => Simple Traversal b Bool
traverseBits f b = Prelude.foldr step 0 <$> traverse g bits
  where
    g n      = (,) n <$> f (testBit b n)
    bits     = Prelude.takeWhile hasBit [0..]
    hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
    step (n,True) r = setBit r n
    step _        r = r

-- this version requires a legal bitSize, and bitSize (undefined :: Integer) will just blow up in our face, 
-- so, I use the version above instead.
--
--traverseBits :: Bits b => Simple Traversal b Bool
--traverseBits f b = snd . Prelude.foldr step (bitSize b - 1,0) <$> traverse (f . testBit b) [0 .. bitSize b - 1] where
--  step True (n,r) = (n - 1, setBit r n)
--  step _    (n,r) = (n - 1, r)

------------------------------------------------------------------------------
-- Cloning Lenses
------------------------------------------------------------------------------

-- | Cloning a 'Lens' is one way to make sure you arent given
-- something weaker, such as a 'Traversal' and can be used
-- as a way to pass around lenses that have to be monomorphic in 'f'.
clone :: Functor f =>
   ((c -> IndexedStore c d d) -> a -> IndexedStore c d b) ->
  (c -> f d) -> a -> f b
clone f cfd a = case f (IndexedStore id) a of
  IndexedStore db c -> db <$> cfd c
{-# INLINE clone #-}
