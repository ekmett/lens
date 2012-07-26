{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012 Edward Kmett
--                (C) 2012 Dan Burton
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
-- getters, multilenses, multi-getters, and multi-lens families in such
-- a way that they can all be composed automatically with @(.)@.
--
----------------------------------------------------------------------------
module Control.Lens
  (
  -- * Lenses
    Lens
  , LensFamily
  , Getter
  , Setter
  , SetterFamily
  , MultiLens
  , MultiLensFamily

  -- * Constructing Lenses
  , makeLenses
  , makeLensesBy
  , makeLensesFor
  , lens
  , iso
  , clone
  , getting
  , gettingMany
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
  , fstL
  , sndL
  , keyL
  , intKeyL
  , memberL
  , intMemberL
  , identityL
  , atL

  -- * MultiGetters
  , folded

  -- ** MultiGetterFamily Combinators
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

  -- * MultiLenses
  , constML
  , keyML
  , intKeyML
  , headML
  , tailML
  , leftML
  , rightML
  , elementML

  -- ** MultiLens Combinators
  , traverseOf
  , mapMOf
  , sequenceAOf
  , sequenceOf

  -- * Implementation details
  , IndexedStore
  , Focusing
  , Traversal
  ) where

import           Control.Applicative              as Applicative
import           Control.Monad (liftM, MonadPlus(..))
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State.Lazy   as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Reader
import           Data.Char (toLower)
import           Data.Foldable                    as Foldable
import           Data.Functor.Identity
import           Data.IntMap                      as IntMap
import           Data.IntSet                      as IntSet
import           Data.Map                         as Map
import           Data.Monoid
import           Data.Set                         as Set
import           Data.Traversable
import           Language.Haskell.TH

infixl 8 ^.
infixr 4 ^%=, ^=, ^+=, ^*=, ^-=, ^/=, ^&&=, ^||=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=
infixr 0 ^$

-- |
-- A Lens is a purely functional reference to part of a data structure, it can be used to read or write to that part of the whole.
--
-- With great power comes great responsibility, and a 'Lens' is subject to the lens laws:
--
-- > reading l (writing l b a)   = b
-- > writing l (reading l a) a   = a
-- > writing l c (writing l b a) = writing l c a
--
-- Every 'Lens' can be used directly as a 'LensFamily' or as a 'Getter', 'Setter', or 'MultiLens', which transitively mens it can be used as
-- almost anything! Such as a 'MultiLensFamily', a 'GetterFamily', a 'MultiGetterFamily', a 'MultiGetter', or a 'SetterFamily'.
--
-- > type Lens a b             = LensFamily a a b b
--
type Lens a b                  = forall f. Functor f => (b -> f b) -> a -> f a

-- | A LensFamily is a more general form of a Lens that permits polymorphic field updates
--
-- With great power comes great responsibility, and a 'LensFamily' is subject to the lens laws:
--
-- > reading l (writing l b a)   = b
-- > writing l (reading l a) a   = a
-- > writing l c (writing l b a) = writing l c a
--
-- These laws are strong enough that the 4 type parameters of a LensFamily cannot vary fully independently. For more on
-- how they interact, read the "Why is it a Lens Family?" section of <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'LensFamily' can be used as a 'GetterFamily', a 'SetterFamily' or a 'MultiLensFamily', which transitively means it can be
-- used as a 'MultiGetterFamily'.
--
-- Despite the complicated signature the pattern for implementing a 'LensFamily' is the same as a Lens.
-- in fact the implementation doesn't change, the type signature merely generalizes.
--
-- > sndL :: LensFamily (c,a) (c,b) a b
-- > sndL f (a,c) = (,) a <$> f c
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
-- since 'reading' a SetterFamily doesn't work, so the other two laws can never be invoked.
type SetterFamily a b c d           = (c -> Identity d) -> a -> Identity b

-- | Every 'Setter' can be used directly as a 'SetterFamily'.
--
-- > type Setter a b                = SetterFamily a a b b
type Setter a b                     = (b -> Identity b) -> a -> Identity a

-- | A 'MultiGetterFamily' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A 'MultiGetterFamily a b c d' provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other MultiGetterFamily combinators.
--
type MultiGetterFamily a b c d      = forall m. Monoid m => (c -> Const m d) -> a -> Const m b

-- | Every 'MultiGetter' can be used directly as a 'MultiGetterFamily'.
--
--
-- > type MultiGetter a b           = MultiGetterFamily a b c d
type MultiGetter a b                = forall m. Monoid m => (b -> Const m b)-> a -> Const m a

-- | A 'GetterFamily' describes how to retrieve a single value in a way that can be composed with
-- other lens-like constructions. It can be used directly as a 'MultiGetterFamily', since it just
-- ignores the 'Monoid'.
type GetterFamily a b c d      = forall z. (c -> Const z d) -> a -> Const z b

-- | A 'Getter' can be used directly as a 'GetterFamily' or as a 'MultiGetter', and hence it can be as a 'MutliGetterFamily'.
--
-- In general while your combinators may produce a 'Getter' it is better to consume any 'GetterFamily'.
--
-- > type Getter a b           = GetterFamily a a b b
type Getter a b                = forall z. (b -> Const z b) -> a -> Const z a

-- | A 'MultiLensFamily' can be used directly as a 'SetterFamily' or a 'MultiGetterFamily' and provides
-- the ability to both read and update multiple fields, subject to the (relatively weak) MultiLensFamily laws.
type MultiLensFamily a b c d        = forall f. Applicative f => (c -> f d) -> a -> f b

-- | Every 'MultiLens' can be used as a 'MultiLensFamily' or a 'Setter' or 'MultiGetter', so it can transitively be used as a
-- 'MultiGetterFamily' or 'SetterFamily' as well.
--
-- > type MultiLens a b             = MultiLensFamily a a b b
type MultiLens a b                  = forall f. Applicative f => (b -> f b) -> a -> f a

-- | Build a 'Lens' or 'LensFamily' from a getter and a setter
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

-- | Building a MultiGetter or MultiGetterFamily
gettingMany :: Foldable f => (a -> f c) -> MultiGetterFamily a b c d
gettingMany f g a = Const (foldMap (getConst . g) (f a))
{-# INLINE gettingMany #-}

-- | Build a Setter or SetterFamily
setting :: ((c -> d) -> a -> b) -> SetterFamily a b c d
setting f g a = Identity (f (runIdentity . g) a)
{-# INLINE setting #-}

------------------------------------------------------------------------------
-- Using Lenses
------------------------------------------------------------------------------

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily' or the fold of a
-- 'MultiGetter', 'MultiLens' or 'MultiLensFamily' that points at monoidal
-- values.
reading :: ((c -> Const c d) -> a -> Const c b) -> a -> c
reading l a = getConst (l Const a)
{-# INLINE reading #-}

-- | Modify the target of a 'Lens', 'LensFamily' or all the targets of a
-- 'Multilens', 'MultiLensFamily', 'Setter' or 'SetterFamily'
mapOf, modifying :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
mapOf l f a = runIdentity (l (Identity . f) a)
modifying = mapOf
{-# INLINE mapOf #-}
{-# INLINE modifying #-}

-- | Replace the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
writing :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
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
-- > ghci> ((0, 1 :+ 2), 3)^.fstL.sndL.getting magnitude
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
-- > ghci> fstL ^+= 1 $ (1,2)
-- > (2,2)
(^+=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^+= n = mapOf l (+ n)
{-# INLINE (^+=) #-}

-- | Multiply the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> sndL ^*= 4 $ (1,2)
-- > (1,8)
(^*=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^-= n = mapOf l (`subtract` n)
{-# INLINE (^-=) #-}

-- | Decrement the target(s) of a numerically valued 'Lens' or 'Setter'
--
-- > ghci> fstL ^-= 2 $ (1,2)
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
-- something weaker, such as a 'MultiLens' or 'MultiLensFamily', and can be used
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

-- > ghci> (1,2)^.fstL
-- > 1
--
-- > ghci> fstL ^= "hello" $ (1,2)
-- > ("hello",2)
fstL :: LensFamily (a,c) (b,c) a b
fstL f (a,c) = (\b -> (b,c)) <$> f a
{-# INLINE fstL #-}

-- | As 'fstL', but for the second field of a pair.
sndL :: LensFamily (c,a) (c,b) a b
sndL f (c,a) = (,) c <$> f a
{-# INLINE sndL #-}

-- | This lens can be used to read, write or delete a member of a 'Map'.
--
-- > ghci> Map.fromList [("hello",12)] ^. keyL "hello"
-- > Just 12
keyL :: Ord k => k -> Lens (Map k v) (Maybe v)
keyL k f m = go <$> f (Map.lookup k m) where
  go Nothing   = Map.delete k m
  go (Just v') = Map.insert k v' m
{-# INLINE keyL #-}

-- | This lens can be used to read, write or delete a member of an 'IntMap'.
--
-- > ghci> IntMap.fromList [(1,"hello")]  ^. keyL 1
-- > Just "hello"
--
-- > ghci> keyL 2 ^= "goodbye" $ IntMap.fromList [(1,"hello")]
-- > fromList [(1,"hello"),(2,"goodbye")]
intKeyL :: Int -> Lens (IntMap v) (Maybe v)
intKeyL k f m = go <$> f (IntMap.lookup k m) where
  go Nothing   = IntMap.delete k m
  go (Just v') = IntMap.insert k v' m
{-# INLINE intKeyL #-}


-- | This lens can be used to read, write or delete a member of a 'Set'
--
-- > ghci> memberL 3 ^= False $ Set.fromList [1,2,3,4]
-- > fromList [1,2,4]
memberL :: Ord k => k -> Lens (Set k) Bool
memberL k f s = go <$> f (Set.member k s) where
  go False = Set.delete k s
  go True  = Set.insert k s
{-# INLINE memberL #-}

-- | This lens can be used to read, write or delete a member of an 'IntSet'
--
-- > ghci> intMemberL 3 ^= False $ IntSet.fromList [1,2,3,4]
-- > fromList [1,2,4]
intMemberL :: Int -> Lens IntSet Bool
intMemberL k f s = go <$> f (IntSet.member k s) where
  go False = IntSet.delete k s
  go True  = IntSet.insert k s
{-# INLINE intMemberL #-}

-- | This lens can be used to access the contents of the Identity monad
identityL :: LensFamily (Identity a) (Identity b) a b
identityL f (Identity a) = Identity <$> f a
{-# INLINE identityL #-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
--
atL :: Eq e => e -> Lens (e -> a) a
atL e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE atL #-}

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
l ~= b = modify (l ^= b)
{-# INLINE (~=) #-}

-- | Modify the value of a field in our monadic state
(%=) :: MonadState a m => Setter a b -> (b -> b) -> m ()
l %= f = modify (l ^%= f)
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
-- Multigetter combinators
--------------------------

-- | > foldMapOf :: Monoid m => MultiGetterFamily a b c d -> (c -> m) -> a -> m
foldMapOf :: Monoid m => ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

-- | > foldOf :: Monoid m => MultiGetterFamily a b m d -> a -> m
foldOf :: Monoid m => ((m -> Const m d) -> a -> Const m b) -> a -> m
foldOf l = getConst . l Const
{-# INLINE foldOf #-}

-- | > foldrOf :: MultiGetterFamily a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf :: ((c -> Const (Endo e) d) -> a -> Const (Endo e) b) -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- | > toListOf :: MultiGetterFamily a b c d -> a -> [c]
toListOf :: ((c -> Const [c] d) -> a -> Const [c] b) -> a -> [c]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- | > andOf :: MultiGetterFamily a b Bool d -> a -> Bool
andOf :: ((Bool -> Const All d) -> a -> Const All b) -> a -> Bool
andOf l = getAll . foldMapOf l All
{-# INLINE andOf #-}

-- | > orOf :: MultiGetterFamily a b Bool d -> a -> Bool
orOf :: ((Bool -> Const Any d) -> a -> Const Any b) -> a -> Bool
orOf l = getAny . foldMapOf l Any
{-# INLINE orOf #-}

-- | > anyOf :: MultiGetterFamily a b c d -> (c -> Bool) -> a -> Bool
anyOf :: ((c -> Const Any d) -> a -> Const Any b) -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
{-# INLINE anyOf #-}

-- | > allOf :: MultiGetterFamily a b c d -> (c -> Bool) -> a -> Bool
allOf :: ((c -> Const All d) -> a -> Const All b) -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
{-# INLINE allOf #-}

-- | > productOf ::  Num c => MultiGetterFamily a b c d -> a -> c
productOf :: Num c => ((c -> Const (Product c) d) -> a -> Const (Product c) b) -> a -> c
productOf l = getProduct . foldMapOf l Product
{-# INLINE productOf #-}

-- | > sumOf ::  Num c => MultiGetterFamily a b c d -> a -> c
sumOf ::  Num c => ((c -> Const (Sum c) d) -> a -> Const (Sum c) b) -> a -> c
sumOf l = getSum . foldMapOf l Sum
{-# INLINE sumOf #-}

-- | > traverseOf_ :: Applicative f => MultiGetterFamily a b c d -> (c -> f e) -> a -> f ()
traverseOf_ :: Applicative f => ((c -> Const (Traversal f) d) -> a -> Const (Traversal f) b) -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversal . foldMapOf l (Traversal . (() <$) . f)
{-# INLINE traverseOf_ #-}

-- | > forOf_ :: Applicative f => MultiGetterFamily a b c d -> a -> (c -> f e) -> f ()
forOf_ :: Applicative f => ((c -> Const (Traversal f) d) -> a -> Const (Traversal f) b) -> a -> (c -> f e) -> f ()
forOf_ l a f = traverseOf_ l f a
{-# INLINE forOf_ #-}

-- | > sequenceAOf_ :: Applicative f => MultiGetterFamily a b (f ()) d -> a -> f ()
sequenceAOf_ :: Applicative f => ((f () -> Const (Traversal f) d) -> a -> Const (Traversal f) b) -> a -> f ()
sequenceAOf_ l = getTraversal . foldMapOf l (Traversal . (() <$))
{-# INLINE sequenceAOf_ #-}

-- | > mapMOf_ :: Monad m => MultiGetterFamily a b c d -> (c -> m e) -> a -> m ()
mapMOf_ :: Monad m => ((c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> (c -> m e) -> a -> m ()
mapMOf_ l f = unwrapMonad . traverseOf_ l (WrapMonad . f)
{-# INLINE mapMOf_ #-}

-- | > forMOf_ :: Monad m => MultiGetterFamily a b c d -> a -> (c -> m e) -> m ()
forMOf_ :: Monad m => ((c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> a -> (c -> m e) -> m ()
forMOf_ l a f = mapMOf_ l f a
{-# INLINE forMOf_ #-}

-- | > sequenceOf_ :: Monad m => MultiGetterFamily a b (m b) d -> a -> m ()
sequenceOf_ :: Monad m => ((m c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> a -> m ()
sequenceOf_ l = unwrapMonad . traverseOf_ l WrapMonad
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > asumOf :: Alternative f => MultiGetterFamily a b c d -> a -> f c
asumOf :: Alternative f => ((f c -> Const (Endo (f c)) d) -> a -> Const (Endo (f c)) b) -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > msumOf :: MonadPlus m => MultiGetterFamily a b c d -> a -> m c
msumOf :: MonadPlus m => ((m c -> Const (Endo (m c)) d) -> a -> Const (Endo (m c)) b) -> a -> m c
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- | > elemOf :: Eq c => MultiGetterFamily a b c d -> c -> a -> Bool
elemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | > notElemOf :: Eq c => MultiGetterFamily a b c d -> c -> a -> Bool
notElemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
notElemOf l c = not . elemOf l c
{-# INLINE notElemOf #-}

-- | > concatMapOf :: MultiGetterFamily a b c d -> (c -> [e]) -> a -> [e]
concatMapOf :: ((c -> Const [e] d) -> a -> Const [e] b) -> (c -> [e]) -> a -> [e]
concatMapOf l ces a = getConst  (l (Const . ces) a)
{-# INLINE concatMapOf #-}

concatOf :: (([e] -> Const [e] d) -> a -> Const [e] b) -> a -> [e]
concatOf = reading
{-# INLINE concatOf #-}

--------------------------
-- Multilens combinators
--------------------------

traverseOf :: Applicative f => ((c -> f d) -> a -> f b) -> (c -> f d) -> a -> f b
traverseOf = id
{-# INLINE traverseOf #-}

mapMOf :: Monad m => ((c -> WrappedMonad m d) -> a -> WrappedMonad m b) -> (c -> m d) -> a -> m b
mapMOf l cmd a = unwrapMonad (l (WrapMonad . cmd) a)
{-# INLINE mapMOf #-}

sequenceAOf :: Applicative f => ((f b -> f (f b)) -> a -> f b) -> a -> f b
sequenceAOf l = l pure
{-# INLINE sequenceAOf #-}

sequenceOf :: Monad m => ((m b -> WrappedMonad m (m b)) -> a -> WrappedMonad m b) -> a -> m b
sequenceOf l = unwrapMonad . l pure
{-# INLINE sequenceOf #-}

--------------------------
-- Multigetters
--------------------------

folded :: Foldable f => MultiGetterFamily (f c) b c d
folded = gettingMany id
{-# INLINE folded #-}

--------------------------
-- Multilenses
--------------------------

-- | This is the partial lens that never succeeds at returning any values
--
-- > constML :: Applicative f => (c -> f d) -> a -> f a
constML :: MultiLensFamily a a c d
constML = const pure
{-# INLINE constML #-}

-- The multilens for reading and writing to the head of a list
--
-- | > headML :: Applicative f => (a -> f a) -> [a] -> f [a]
headML :: MultiLens [a] a
headML _ [] = pure []
headML f (a:as) = (:as) <$> f a
{-# INLINE headML #-}

-- The multilens for reading and writing to the tail of a list
--
-- | > tailML :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
tailML :: MultiLens [a] [a]
tailML _ [] = pure []
tailML f (a:as) = (a:) <$> f as
{-# INLINE tailML #-}

-- | A multilens for tweaking the left-hand value in an Either:
--
-- > leftML :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
leftML :: MultiLensFamily (Either a c) (Either b c) a b
leftML f (Left a)  = Left <$> f a
leftML _ (Right c) = pure $ Right c
{-# INLINE leftML #-}

-- | A multilens for tweaking the right-hand value in an Either:
--
-- > rightML :: Applicative f => (a -> f b) -> Either c a -> f (Either c a)
-- > rightML = traverse
--
-- Unfortunately the instance for 'Traversable (Either c)' is still missing from
-- base.
rightML :: MultiLensFamily (Either c a) (Either c b) a b
rightML _ (Left c) = pure $ Left c
rightML f (Right a) = Right <$> f a
{-# INLINE rightML #-}

-- |
-- > keyML :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > keyML k = keyL k . traverse
keyML :: Ord k => k -> MultiLens (Map k v) v
keyML k = keyL k . traverse
{-# INLINE keyML #-}

-- |
-- > intKeyML :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > intKeyML k = intKeyL k . traverse
intKeyML :: Int -> MultiLens (IntMap v) v
intKeyML k = intKeyL k . traverse
{-# INLINE intKeyML #-}

-- | > elementML :: (Applicative f, Traversable t) => Int -> (a -> f a) -> t a -> f (t a)
elementML :: Traversable t => Int -> MultiLens (t a) a
elementML j f ta = fst (runSA (traverse go ta) 0) where
  go a = SA $ \i -> (if i == j then f a else pure a, i + 1)
{-# INLINE elementML #-}

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

newtype Traversal f = Traversal { getTraversal :: f () }

instance Applicative f => Monoid (Traversal f) where
  mempty = Traversal (pure ())
  Traversal ma `mappend` Traversal mb = Traversal (ma *> mb)

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
