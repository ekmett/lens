{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012 Edward Kmett, Dan Burton
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  RankNTypes, TemplateHaskell
--
-- This package provides lenses that are compatible with other van
-- Laarhoven lens libraries, while reducing the complexty of the imports.
--
-- Lenses produced by this library are compatible with other van Laarhoven
-- lens family libraries, such as lens-family, lens-family-core and
-- lens-family-th, but the API is simpler.
--
-- Note: If you merely want your library to _provide_ lenses you may not have
-- to actually import _any_ lens library, for a @'Lens' Bar Foo@, just export
-- a function with the signature:
--
-- > foo :: Functor f => (Foo -> f Foo) -> Bar -> f Bar
--
-- and then you can compose it with other lenses using @(.)@.
--
-- This package provides lenses, lens families, setters, setter families,
-- getters, traversals, folds, and traversal families in such
-- a way that they can all be composed automatically with @(.)@.
--
----------------------------------------------------------------------------
module Control.Lens
  (
  -- * Lenses
    Lens, LensFamily
  , Getter, GetterFamily
  , Setter, SetterFamily
  , Fold, FoldFamily
  , Traversal, TraversalFamily

  -- * Constructing Lenses
  , makeLenses
  , makeLensesBy
  , makeLensesFor
  , lens
  , iso
  , clone
  , getting
  , folding
  , setting

  -- * Manipulating Values
  , reading
  , modifying
  , writing
  , (^.), (^$)
  , (^%=), (^=), (^+=), (^-=), (^*=), (^/=), (^||=), (^&&=)

  -- * Manipulating State
  , access
  , Focus(..)
  , (%=), (~=), (%%=), (+=), (-=), (*=), (//=), (||=), (&&=)

  -- * Lenses and LensFamilies
  , _1
  , _2
  , valueAt
  , valueAtInt
  , contains
  , containsInt
  , identity
  , resultAt

  -- * Folds
  , folded

  -- ** Fold Combinators
  , mapOf
  , foldMapOf
  , foldrOf
  , foldOf
  , toListOf
  , anyOf, allOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_
  , forOf_
  , sequenceAOf_
  , mapMOf_
  , forMOf_
  , sequenceOf_
  , asumOf, msumOf
  , concatMapOf
  , concatOf
  , elemOf
  , notElemOf

  -- * Traversals
  , traverseNothing
  , traverseKey
  , traverseIntKey
  , traverseHead
  , traverseTail
  , traverseLeft
  , traverseRight
  , traverseElement
  , TraverseByteString(..)

  -- ** Traversal Combinators
  , traverseOf
  , mapMOf
  , sequenceAOf
  , sequenceOf

  -- * Implementation details
  , IndexedStore
  , Focusing
  , Traversed
  ) where

import           Control.Applicative              as Applicative
import           Control.Monad (liftM, MonadPlus(..))
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State.Lazy   as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy             as Lazy
import           Data.ByteString                  as Strict
import           Data.Char (toLower)
import           Data.Foldable                    as Foldable
import           Data.Functor.Identity
import           Data.IntMap                      as IntMap
import           Data.IntSet                      as IntSet
import           Data.Map                         as Map
import           Data.Monoid
import           Data.Set                         as Set
import           Data.Traversable
import           Data.Word (Word8)
import           Language.Haskell.TH

infixl 8 ^.
infixr 4 ^%=, ^=, ^+=, ^*=, ^-=, ^/=, ^&&=, ^||=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=
infixr 0 ^$

-- |
-- A Lens is a purely functional reference to part of a data structure. It can be used to read or write to that part of the whole.
--
-- With great power comes great responsibility, and a 'Lens' is subject to the lens laws:
--
-- > reading l (writing l b a)   = b
-- > writing l (reading l a) a   = a
-- > writing l c (writing l b a) = writing l c a
--
-- Every 'Lens' can be used directly as a 'LensFamily' or as a 'Getter', 'Setter', or 'Traversal', which transitively mens it can be used as
-- almost anything! Such as a 'TraversalFamily', a 'GetterFamily', a 'FoldFamily', a 'Fold', or a 'SetterFamily'.
--
-- > type Lens a b             = LensFamily a a b b
--
type Lens a b                  = forall f. Functor f => (b -> f b) -> a -> f a

-- | A 'LensFamily' is a more general form of a 'Lens' that permits polymorphic field updates
--
-- With great power comes great responsibility, and a 'LensFamily' is subject to the lens laws:
--
-- > reading l (writing l b a)   = b
-- > writing l (reading l a) a   = a
-- > writing l c (writing l b a) = writing l c a
--
-- These laws are strong enough that the 4 type parameters of a 'LensFamily' cannot vary fully independently. For more on
-- how they interact, read the "Why is it a Lens Family?" section of <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'LensFamily' can be used as a 'GetterFamily', a 'SetterFamily' or a 'TraversalFamily', which transitively means it can be
-- used as a 'FoldFamily'.
--
-- Despite the complicated signature the pattern for implementing a 'LensFamily' is the same as a Lens.
-- in fact the implementation doesn't change, the type signature merely generalizes.
--
-- > _2 :: LensFamily (c,a) (c,b) a b
-- > _2 f (a,c) = (,) a <$> f c
type LensFamily a b c d        = forall f. Functor f => (c -> f d) -> a -> f b

-- | A 'SetterFamily' describes a way to perform polymorphic update to potentially multiple fields in a way that can be
-- composed with other lens-like constructions that can be used as a 'SetterFamily'.
--
-- The typical way to obtain a 'SetterFamily' is to build one with 'setting' or to compose some other lens-like construction
-- with a 'SetterFamily'.
--
-- Note: the only lens law that applies to a 'SetterFamily' is
--
-- > writing l c (writing l b a) = writing l c a
--
-- 'reading' a 'SetterFamily' doesn't work, so the other two laws can never be invoked.
type SetterFamily a b c d           = (c -> Identity d) -> a -> Identity b

-- | Every 'Setter' can be used directly as a 'SetterFamily'.
--
-- > type Setter a b                = SetterFamily a a b b
type Setter a b                     = (b -> Identity b) -> a -> Identity a

-- | A 'FoldFamily' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A 'FoldFamily a b c d' provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other FoldFamily combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f c)@, then there should be a
-- 'fooOf' method that takes a @'FoldFamily' a b c d@ and a value of type @a@.
--
type FoldFamily a b c d      = forall m. Monoid m => (c -> Const m d) -> a -> Const m b

-- | Every 'Fold' can be used directly as a 'FoldFamily'.
--
-- > type Fold a b           = FoldFamily a b c d
type Fold a b                = forall m. Monoid m => (b -> Const m b)-> a -> Const m a

-- | A 'GetterFamily' describes how to retrieve a single value in a way that can be composed with
-- other lens-like constructions. It can be used directly as a 'FoldFamily', since it just
-- ignores the 'Monoid'.
type GetterFamily a b c d      = forall z. (c -> Const z d) -> a -> Const z b

-- | A 'Getter' can be used directly as a 'GetterFamily' or as a 'Fold', and hence it can be as a 'MutliGetterFamily'.
--
-- In general while your combinators may produce a 'Getter' it is better to consume any 'GetterFamily'.
--
-- > type Getter a b           = GetterFamily a a b b
type Getter a b                = forall z. (b -> Const z b) -> a -> Const z a

-- | A 'TraversalFamily' can be used directly as a 'SetterFamily' or a 'FoldFamily' and provides
-- the ability to both read and update multiple fields, subject to the (relatively weak) 'TraversalFamily' laws.
--
-- These are also known as @MultiLens@ families, but they have the signature and spirit of
--
-- > traverse :: Traversable f => TraversalFamiy (f a) (f b) a b
--
-- and the more evocative name suggests their application.
type TraversalFamily a b c d        = forall f. Applicative f => (c -> f d) -> a -> f b

-- | Every 'Traversal' can be used as a 'TraversalFamily' or a 'Setter' or 'Fold', so it can transitively be used as a
-- 'FoldFamily' or 'SetterFamily' as well.
--
-- > type Traversal a b             = TraversalFamily a a b b
type Traversal a b                  = forall f. Applicative f => (b -> f b) -> a -> f a

-- | Build a 'Lens' or 'LensFamily' from a getter and a setter.
--
-- > lens :: Functor f => (a -> c) -> (d -> a -> b) -> (c -> f d) -> a -> f b
lens :: (a -> c) -> (d -> a -> b) -> LensFamily a b c d
lens ac dab cfd a = (`dab` a) <$> cfd (ac a)
{-# INLINE lens #-}

-- | Built a 'Lens' or 'LensFamily' from an isomorphism or an isomorphism family
--
-- > iso :: Functor f => (a -> c) -> (d -> b) -> (c -> f d) -> a -> f b
iso :: (a -> c) -> (d -> b) -> LensFamily a b c d
iso f g h a = g <$> h (f a )
{-# INLINE iso #-}

-- | Build a Getter or GetterFamily
getting :: (a -> c) -> GetterFamily a b c d
getting f g a = Const (getConst (g (f a)))
{-# INLINE getting #-}

-- | Building a Fold or FoldFamily
folding :: Foldable f => (a -> f c) -> FoldFamily a b c d
folding f g a = Const (foldMap (getConst . g) (f a))
{-# INLINE folding #-}

-- | Build a Setter or SetterFamily
setting :: ((c -> d) -> a -> b) -> SetterFamily a b c d
setting f g a = Identity (f (runIdentity . g) a)
{-# INLINE setting #-}

------------------------------------------------------------------------------
-- Using Lenses
------------------------------------------------------------------------------

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily' or the fold of a
-- 'Fold', 'Traversal' or 'TraversalFamily' that points at monoidal
-- values.
--
-- > reading :: GetterFamily a b c d -> a -> c
reading :: ((c -> Const c d) -> a -> Const c b) -> a -> c
reading l a = getConst (l Const a)
{-# INLINE reading #-}

-- | Modify the target of a 'Lens', 'LensFamily' or all the targets of a
-- 'Multilens', 'TraversalFamily', 'Setter' or 'SetterFamily'
--
-- > modifying :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
-- > modifying = mapOf
modifying :: SetterFamily a b c d -> (c -> d) -> a -> b
modifying l f a = runIdentity (l (Identity . f) a)
{-# INLINE mapOf #-}

-- | Modify the target of a 'Lens', 'LensFamily' or all the targets of a
-- 'Multilens', 'TraversalFamily', 'Setter' or 'SetterFamily'
--
-- > mapOf :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
-- > mapOf = modifying
mapOf :: SetterFamily a b c d -> (c -> d) -> a -> b
mapOf l f a = runIdentity (l (Identity . f) a)
{-# INLINE modifying #-}

-- | Replace the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
--
-- writing :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
writing :: SetterFamily a b c d -> d -> a -> b
writing l d a = runIdentity (l (\_ -> Identity d) a)
{-# INLINE writing #-}

-- | Read the value of a 'Getter', 'Lens' or 'LensFamily'.
-- This is the same operation as 'reading'.
(^$) :: ((c -> Const c d) -> a -> Const c b) -> a -> c
l ^$ a = getConst (l Const a)
{-# INLINE (^$) #-}

-- | Read a field from a 'Getter', 'Lens' or 'LensFamily'.
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with (Prelude..) This is the same operation as 'flip reading'
--
-- > ghci> ((0, 1 :+ 2), 3)^._1._2.getting magnitude
-- > 2.23606797749979
(^.) :: a -> ((c -> Const c d) -> a -> Const c b) -> c
a ^. l = getConst (l Const a)
{-# INLINE (^.) #-}

-- | Modifies the target of a 'Lens', 'LensFamily', 'Setter', or 'SetterFamily'.
--
-- This is an infix version of 'mapOf'
(^%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
l ^%= f = runIdentity . l (Identity . f)
{-# INLINE (^%=) #-}

-- | Replaces the target(s) of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'.
--
-- This is an infix version of 'writing'
(^=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
l ^= v = runIdentity . l (Identity . const v)
{-# INLINE (^=) #-}

-- | Increment the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> _1 ^+= 1 $ (1,2)
-- > (2,2)
(^+=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^+= n = mapOf l (+ n)
{-# INLINE (^+=) #-}

-- | Multiply the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> _2 ^*= 4 $ (1,2)
-- > (1,8)
(^*=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^-= n = mapOf l (`subtract` n)
{-# INLINE (^-=) #-}

-- | Decrement the target(s) of a numerically valued 'Lens' or 'Setter'
--
-- > ghci> _1 ^-= 2 $ (1,2)
-- > (-1,2)
(^-=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^*= n = mapOf l (* n)
{-# INLINE (^*=) #-}

-- | Divide the target(s) of a numerically valued 'Lens' or 'Setter'
(^/=) :: Fractional c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^/= n = mapOf l (/ n)

-- | Logically '||' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(^||=):: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
l ^||= n = mapOf l (|| n)
{-# INLINE (^||=) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(^&&=) :: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
l ^&&= n = mapOf l (&& n)
{-# INLINE (^&&=) #-}

------------------------------------------------------------------------------
-- Cloning Lenses
------------------------------------------------------------------------------

data IndexedStore c d a = IndexedStore (d -> a) c

instance Functor (IndexedStore c d) where
  fmap f (IndexedStore g c) = IndexedStore (f . g) c

-- | Cloning a 'Lens' or 'LensFamily' is one way to make sure you arent given
-- something weaker, such as a 'Traversal' or 'TraversalFamily', and can be used
-- as a way to pass around lenses that have to be monomorphic in 'f'.
clone :: Functor f => ((c -> IndexedStore c d d) -> a -> IndexedStore c d b) -> (c -> f d) -> a -> f b
clone f cfd a = case f (IndexedStore id) a of
  IndexedStore db c -> db <$> cfd c
{-# INLINE clone #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | This is a lens family that can change the value (and type) of the first field of
-- a pair.

-- > ghci> (1,2)^._1
-- > 1
--
-- > ghci> _1 ^= "hello" $ (1,2)
-- > ("hello",2)
--
-- > anyOf _2 :: (c -> Bool) -> (a, c) -> Bool
-- > traverse._2 :: (Applicative f, Traversable t) => (a -> f b) -> t (c, a) -> f (t (c, b))
-- > foldMapOf (traverse._2) :: (Traversable t, Monoid m) => (c -> m) -> t (b, c) -> m
_1 :: LensFamily (a,c) (b,c) a b
_1 f (a,c) = (\b -> (b,c)) <$> f a
{-# INLINE _1 #-}

-- | As '_1', but for the second field of a pair.
_2 :: LensFamily (c,a) (c,b) a b
_2 f (c,a) = (,) c <$> f a
{-# INLINE _2 #-}

-- | This lens can be used to read, write or delete a member of a 'Map'.
--
-- > ghci> Map.fromList [("hello",12)] ^. valueAt "hello"
-- > Just 12
valueAt :: Ord k => k -> Lens (Map k v) (Maybe v)
valueAt k f m = go <$> f (Map.lookup k m) where
  go Nothing   = Map.delete k m
  go (Just v') = Map.insert k v' m
{-# INLINE valueAt #-}

-- | This lens can be used to read, write or delete a member of an 'IntMap'.
--
-- > ghci> IntMap.fromList [(1,"hello")]  ^. valueAt 1
-- > Just "hello"
--
-- > ghci> valueAt 2 ^= "goodbye" $ IntMap.fromList [(1,"hello")]
-- > fromList [(1,"hello"),(2,"goodbye")]
valueAtInt :: Int -> Lens (IntMap v) (Maybe v)
valueAtInt k f m = go <$> f (IntMap.lookup k m) where
  go Nothing   = IntMap.delete k m
  go (Just v') = IntMap.insert k v' m
{-# INLINE valueAtInt #-}


-- | This lens can be used to read, write or delete a member of a 'Set'
--
-- > ghci> contains 3 ^= False $ Set.fromList [1,2,3,4]
-- > fromList [1,2,4]
contains :: Ord k => k -> Lens (Set k) Bool
contains k f s = go <$> f (Set.member k s) where
  go False = Set.delete k s
  go True  = Set.insert k s
{-# INLINE contains #-}

-- | This lens can be used to read, write or delete a member of an 'IntSet'
--
-- > ghci> containsInt 3 ^= False $ IntSet.fromList [1,2,3,4]
-- > fromList [1,2,4]
containsInt :: Int -> Lens IntSet Bool
containsInt k f s = go <$> f (IntSet.member k s) where
  go False = IntSet.delete k s
  go True  = IntSet.insert k s
{-# INLINE containsInt #-}

-- | This lens can be used to access the contents of the Identity monad
identity :: LensFamily (Identity a) (Identity b) a b
identity f (Identity a) = Identity <$> f a
{-# INLINE identity #-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
--
resultAt :: Eq e => e -> Lens (e -> a) a
resultAt e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE resultAt #-}

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

-- | Access a field of a state monad
access :: MonadState a m => ((c -> Const c d) -> a -> Const c b) -> m c
access l = gets (^. l)
{-# INLINE access #-}

newtype Focusing m c a = Focusing { unfocusing :: m (c, a) }

instance Monad m => Functor (Focusing m c) where
  fmap f (Focusing m) = Focusing (liftM (fmap f) m)

instance (Monad m, Monoid c) => Applicative (Focusing m c) where
  pure a = Focusing (return (mempty, a))
  Focusing mf <*> Focusing ma = Focusing $ do
    (c, f) <- mf
    (d, a) <- ma
    return (mappend c d, f a)

-- | This class allows us to use 'focus' on a number of different monad transformers.
class Focus st where
  -- | Use a lens to lift an operation with simpler context into a larger context
  focus :: Monad m => ((b -> Focusing m c b) -> a -> Focusing m c a) -> st b m c -> st a m c

instance Focus Strict.StateT where
  focus l (Strict.StateT m) = Strict.StateT $ \a -> unfocusing (l (Focusing . m) a)
  {-# INLINE focus #-}

instance Focus Lazy.StateT where
  focus l (Lazy.StateT m) = Lazy.StateT $ \a -> unfocusing (l (Focusing . m) a)
  {-# INLINE focus #-}

-- | We can focus Reader environments, too!
instance Focus ReaderT where
  focus l (ReaderT m) = ReaderT $ \a -> liftM undefined $  unfocusing $ l (\b -> Focusing $ (\c -> (c,b)) `liftM` m b) a
  {-# INLINE focus #-}

-- | Set the value of a field in our monadic state
(~=) :: MonadState a m => Setter a b -> b -> m ()
l ~= b = modify $ l ^= b
{-# INLINE (~=) #-}

-- | Modify the value of a field in our monadic state
(%=) :: MonadState a m => Setter a b -> (b -> b) -> m ()
l %= f = modify $ l ^%= f
{-# INLINE (%=) #-}

-- | Modify the value of a field in our monadic state and return some information about it
(%%=) :: MonadState a m => ((b -> (c,b)) -> a -> (c,a)) -> (b -> (c, b)) -> m c
l %%= f = state (l f)
{-# INLINE (%%=) #-}

-- | Modify a numeric field in our monadic state by adding to it
(+=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l += b = modify $ l ^+= b
{-# INLINE (+=) #-}

-- | Modify a numeric field in our monadic state by subtracting from it
(-=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l -= b = modify $ l ^-= b
{-# INLINE (-=) #-}

-- | Modify a numeric field in our monadic state by multiplying it
(*=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l *= b = modify $ l ^*= b
{-# INLINE (*=) #-}

-- | Modify a numeric field in our monadic state by dividing it
(//=) ::  (MonadState a m, Fractional b) => Setter a b -> b -> m ()
l //= b = modify $ l ^/= b
{-# INLINE (//=) #-}

-- | Modify a boolean field in our monadic state by computing its logical '&&' with another value.
(&&=):: MonadState a m => Setter a Bool -> Bool -> m ()
l &&= b = modify $ l ^&&= b
{-# INLINE (&&=) #-}

-- | Modify a boolean field in our monadic state by computing its logical '||' with another value.
(||=) :: MonadState a m => Setter a Bool -> Bool -> m ()
l ||= b = modify $ l ^||= b
{-# INLINE (||=) #-}

--------------------------
-- Fold combinators
--------------------------

-- |
-- > foldMap = foldMapOf folded
--
-- > foldMapOf :: Monoid m => FoldFamily a b c d -> (c -> m) -> a -> m
foldMapOf :: Monoid m => ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

-- |
-- > fold = foldOf folded
--
-- > foldOf :: Monoid m => FoldFamily a b m d -> a -> m
foldOf :: Monoid m => ((m -> Const m d) -> a -> Const m b) -> a -> m
foldOf l = getConst . l Const
{-# INLINE foldOf #-}

-- |
-- > foldr = foldrOf folded
--
-- > foldrOf :: FoldFamily a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf :: ((c -> Const (Endo e) d) -> a -> Const (Endo e) b) -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- |
-- > toList = toListOf folded
--
-- > toListOf :: FoldFamily a b c d -> a -> [c]
toListOf :: ((c -> Const [c] d) -> a -> Const [c] b) -> a -> [c]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- |
-- > and = andOf folded
--
-- > andOf :: FoldFamily a b Bool d -> a -> Bool
andOf :: ((Bool -> Const All d) -> a -> Const All b) -> a -> Bool
andOf l = getAll . foldMapOf l All
{-# INLINE andOf #-}

-- |
-- > or = orOf folded
--
-- > orOf :: FoldFamily a b Bool d -> a -> Bool
orOf :: ((Bool -> Const Any d) -> a -> Const Any b) -> a -> Bool
orOf l = getAny . foldMapOf l Any
{-# INLINE orOf #-}

-- |
-- > any = anyOf folded
--
-- > anyOf :: FoldFamily a b c d -> (c -> Bool) -> a -> Bool
anyOf :: ((c -> Const Any d) -> a -> Const Any b) -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
{-# INLINE anyOf #-}

-- |
-- > all = allOf folded
--
-- > allOf :: FoldFamily a b c d -> (c -> Bool) -> a -> Bool
allOf :: ((c -> Const All d) -> a -> Const All b) -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
{-# INLINE allOf #-}

-- |
-- > product = productOf folded
--
-- > productOf ::  Num c => FoldFamily a b c d -> a -> c
productOf :: Num c => ((c -> Const (Product c) d) -> a -> Const (Product c) b) -> a -> c
productOf l = getProduct . foldMapOf l Product
{-# INLINE productOf #-}

-- |
-- > sum = sumOf folded
--
-- > sumOf ::  Num c => FoldFamily a b c d -> a -> c
sumOf ::  Num c => ((c -> Const (Sum c) d) -> a -> Const (Sum c) b) -> a -> c
sumOf l = getSum . foldMapOf l Sum
{-# INLINE sumOf #-}

-- |
-- > traverse_ = traverseOf_ folded
--
-- > traverseOf_ :: Applicative f => FoldFamily a b c d -> (c -> f e) -> a -> f ()
traverseOf_ :: Applicative f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversed . foldMapOf l (Traversed . (() <$) . f)
{-# INLINE traverseOf_ #-}

-- |
-- > for_ = forOf_ folded
--
-- > forOf_ :: Applicative f => FoldFamily a b c d -> a -> (c -> f e) -> f ()
forOf_ :: Applicative f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> (c -> f e) -> f ()
forOf_ l a f = traverseOf_ l f a
{-# INLINE forOf_ #-}

-- |
-- > sequenceA_ = sequenceAOf_ folded
--
-- > sequenceAOf_ :: Applicative f => FoldFamily a b (f ()) d -> a -> f ()
sequenceAOf_ :: Applicative f => ((f () -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> f ()
sequenceAOf_ l = getTraversed . foldMapOf l (Traversed . (() <$))
{-# INLINE sequenceAOf_ #-}

-- |
-- > mapM_ = mapMOf_ folded
--
-- > mapMOf_ :: Monad m => FoldFamily a b c d -> (c -> m e) -> a -> m ()
mapMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> (c -> m e) -> a -> m ()
mapMOf_ l f = unwrapMonad . traverseOf_ l (WrapMonad . f)
{-# INLINE mapMOf_ #-}

-- |
-- > forM_ = forMOf_ folded
--
-- > forMOf_ :: Monad m => FoldFamily a b c d -> a -> (c -> m e) -> m ()
forMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> (c -> m e) -> m ()
forMOf_ l a f = mapMOf_ l f a
{-# INLINE forMOf_ #-}

-- |
-- > sequence_ = sequenceOf_ folded
--
-- > sequenceOf_ :: Monad m => FoldFamily a b (m b) d -> a -> m ()
sequenceOf_ :: Monad m => ((m c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> m ()
sequenceOf_ l = unwrapMonad . traverseOf_ l WrapMonad
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > asum = asumOf folded
--
-- > asumOf :: Alternative f => FoldFamily a b c d -> a -> f c
asumOf :: Alternative f => ((f c -> Const (Endo (f c)) d) -> a -> Const (Endo (f c)) b) -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > msum = msumOf folded
--
-- > msumOf :: MonadPlus m => FoldFamily a b c d -> a -> m c
msumOf :: MonadPlus m => ((m c -> Const (Endo (m c)) d) -> a -> Const (Endo (m c)) b) -> a -> m c
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- |
-- > elem = elemOf folded
--
-- > elemOf :: Eq c => FoldFamily a b c d -> c -> a -> Bool
elemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- |
-- > notElem = notElemOf folded
--
-- > notElemOf :: Eq c => FoldFamily a b c d -> c -> a -> Bool
notElemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
notElemOf l c = not . elemOf l c
{-# INLINE notElemOf #-}

-- |
-- > concatMap = concatMapOf folded
--
-- > concatMapOf :: FoldFamily a b c d -> (c -> [e]) -> a -> [e]
concatMapOf :: ((c -> Const [e] d) -> a -> Const [e] b) -> (c -> [e]) -> a -> [e]
concatMapOf l ces a = getConst  (l (Const . ces) a)
{-# INLINE concatMapOf #-}

-- |
-- > concat = concatOf folded
--
-- > concatOf :: FoldFamily a b [e] d -> a -> [e]
concatOf :: (([e] -> Const [e] d) -> a -> Const [e] b) -> a -> [e]
concatOf = reading
{-# INLINE concatOf #-}

--------------------------
-- Multilens combinators
--------------------------

-- |
-- > traverseOf = id
-- > traverse = traverseOf traverse
--
-- > traverseOf :: Applicative f => TraversalFamily a b c d -> (c -> f d) -> a -> f b
traverseOf :: Applicative f => ((c -> f d) -> a -> f b) -> (c -> f d) -> a -> f b
traverseOf = id
{-# INLINE traverseOf #-}

-- |
-- > mapM = mapMOf traverse
--
-- > mapM :: Monad m => TraversalFamily a b c d -> (c -> m d) -> a -> m b
mapMOf :: Monad m => ((c -> WrappedMonad m d) -> a -> WrappedMonad m b) -> (c -> m d) -> a -> m b
mapMOf l cmd a = unwrapMonad (l (WrapMonad . cmd) a)
{-# INLINE mapMOf #-}

-- |
-- > sequenceA = sequenceAOf traverse
--
-- > sequenceA :: Applicative f => TraversalFamily a b (f c) (f c) -> a -> f b
sequenceAOf :: Applicative f => ((f c -> f (f c)) -> a -> f b) -> a -> f b
sequenceAOf l = l pure
{-# INLINE sequenceAOf #-}

-- |
-- > sequence = sequenceOf traverse
--
-- > sequence :: Monad m => TraversalFamily a b (m c) (m c) -> a -> m b
sequenceOf :: Monad m => ((m c -> WrappedMonad m (m c)) -> a -> WrappedMonad m b) -> a -> m b
sequenceOf l = unwrapMonad . l pure
{-# INLINE sequenceOf #-}

--------------------------
-- Folds
--------------------------

folded :: Foldable f => FoldFamily (f c) b c d
folded = folding id
{-# INLINE folded #-}

--------------------------
-- Traversals
--------------------------

-- | This is the traversal that never succeeds at returning any values
--
-- > traverseNothing :: Applicative f => (c -> f d) -> a -> f a
traverseNothing :: TraversalFamily a a c d
traverseNothing = const pure
{-# INLINE traverseNothing #-}

-- The traversal for reading and writing to the head of a list
--
-- | > traverseHead :: Applicative f => (a -> f a) -> [a] -> f [a]
traverseHead :: Traversal [a] a
traverseHead _ [] = pure []
traverseHead f (a:as) = (:as) <$> f a
{-# INLINE traverseHead #-}

-- The traversal for reading and writing to the tail of a list
--
-- | > traverseTail :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
traverseTail :: Traversal [a] [a]
traverseTail _ [] = pure []
traverseTail f (a:as) = (a:) <$> f as
{-# INLINE traverseTail #-}

-- | A traversal for tweaking the left-hand value in an Either:
--
-- > traverseLeft :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
traverseLeft :: TraversalFamily (Either a c) (Either b c) a b
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
traverseRight :: TraversalFamily (Either c a) (Either c b) a b
traverseRight _ (Left c) = pure $ Left c
traverseRight f (Right a) = Right <$> f a
{-# INLINE traverseRight #-}

-- | Traverse the value at a given key in a Map
--
-- > traverseKey :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > traverseKey k = valueAt k . traverse
traverseKey :: Ord k => k -> Traversal (Map k v) v
traverseKey k = valueAt k . traverse
{-# INLINE traverseKey #-}

-- | Traverse the value at a given key in an IntMap
--
-- > traverseIntKey :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > traverseIntKey k = valueAtInt k . traverse
traverseIntKey :: Int -> Traversal (IntMap v) v
traverseIntKey k = valueAtInt k . traverse
{-# INLINE traverseIntKey #-}

-- | Traverse a single element in a traversable container.
--
-- > traverseElement :: (Applicative f, Traversable t) => Int -> (a -> f a) -> t a -> f (t a)
traverseElement :: Traversable t => Int -> Traversal (t a) a
traverseElement j f ta = fst (runSA (traverse go ta) 0) where
  go a = SA $ \i -> (if i == j then f a else pure a, i + 1)
{-# INLINE traverseElement #-}

class TraverseByteString t where
  -- | Traverse the individual bytes in a ByteString
  --
  -- > ghci> :t anyOf traverseByteString
  -- anyOf traverseByteString
  --   :: TraverseByteString b => (GHC.Word.Word8 -> Bool) -> b -> Bool
  traverseByteString :: Traversal t Word8

instance TraverseByteString Strict.ByteString where
  traverseByteString f = fmap Strict.pack . traverse f . Strict.unpack

instance TraverseByteString Lazy.ByteString where
  traverseByteString f = fmap Lazy.pack . traverse f . Lazy.unpack

------------------------------------------------------------------------------
-- Implementation details
------------------------------------------------------------------------------

newtype SA f a = SA { runSA :: Int -> (f a, Int) }

instance Functor f => Functor (SA f) where
  fmap f (SA m) = SA $ \i -> case m i of
    (fa, j) -> (fmap f fa, j)

instance Applicative f => Applicative (SA f) where
  pure a = SA (\i -> (pure a, i))
  SA mf <*> SA ma = SA $ \i -> case mf i of
    (ff, j) -> case ma j of
       (fa, k) -> (ff <*> fa, k)

newtype Traversed f = Traversed { getTraversed :: f () }

instance Applicative f => Monoid (Traversed f) where
  mempty = Traversed (pure ())
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)

-- wrapMonadL :: Functor f => (m a -> f (n b)) -> WrappedMonad m a -> f (WrappedMonad n b)
-- wrapMonadL f (WrapMonad ma) = WrapMonad <$> f ma

------------------------------------------------------------------------------
-- Template Haskell
------------------------------------------------------------------------------

-- | By default, if the field name begins with an underscore,
-- then the underscore will simply be removed (and the new first character
-- lowercased if necessary).
defaultNameTransform :: String -> Maybe String
defaultNameTransform ('_':c:rest) = Just $ toLower c : rest
defaultNameTransform _ = Nothing

-- | Information about the larger type the lens will operate on.
type LensTypeInfo = (Name, [TyVarBndr])

-- | Information about the smaller type the lens will operate on.
type ConstructorFieldInfo = (Name, Strict, Type)

-- | Derive lenses with the provided name transformation
-- and filtering function. Produce @Just lensName@ to generate a lens
-- of the resultant name, or @Nothing@ to not generate a lens
-- for the input record name.
--
-- Example usage:
--
-- > makeLensesBy (\n -> Just (n ++ "L")) ''Foo
makeLensesBy ::
     (String -> Maybe String) -- ^ the name transformer
  -> Name -> Q [Dec]
makeLensesBy nameTransform datatype = do
  typeInfo          <- extractLensTypeInfo datatype
  let derive1 = deriveLens nameTransform typeInfo
  constructorFields <- extractConstructorFields datatype
  Prelude.concat <$> Prelude.mapM derive1 constructorFields

extractLensTypeInfo :: Name -> Q LensTypeInfo
extractLensTypeInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ n ts _ _) -> (n, ts)
    TyConI (NewtypeD _ n ts _ _) -> (n, ts)
    _ -> error $ "Can't derive Lens for: "  ++ datatypeStr ++ ", type name required."

extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD    _ _ _ [_]         _) -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI TySynD{}   -> error $ "Can't derive Lens for type synonym: " ++ datatypeStr
    TyConI DataD{}    -> error $ "Can't derive Lens for tagged union: " ++ datatypeStr
    _                 -> error $ "Can't derive Lens for: "  ++ datatypeStr ++ ", type name required."

-- Derive a lens for the given record selector
-- using the given name transformation function.
deriveLens :: (String -> Maybe String)
           -> LensTypeInfo
           -> ConstructorFieldInfo
           -> Q [Dec]
deriveLens nameTransform ty field = case nameTransform (nameBase fieldName) of
  Nothing          -> return []
  Just lensNameStr -> do
    body <- deriveLensBody (mkName lensNameStr) fieldName
    return [body]
  where
    (fieldName, _fieldStrict, _fieldType) = field
    (_tyName, _tyVars) = ty  -- just to clarify what's here

-- Given a record field name,
-- produces a single function declaration:
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
deriveLensBody :: Name -> Name -> Q Dec
deriveLensBody lensName fieldName = funD lensName [defLine]
  where
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(appE (varE f) (appE (varE fieldName) (varE a)))
            |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

-- | Derive lenses for the record selectors in
-- a single-constructor data declaration,
-- or for the record selector in a newtype declaration.
-- Lenses will only be generated for record fields which
-- are prefixed with an underscore.
--
-- Example usage:
--
-- > makeLenses ''Foo
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesBy defaultNameTransform

-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
--
-- Example usage:
--
-- > makeLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesBy (`Prelude.lookup` fields)
