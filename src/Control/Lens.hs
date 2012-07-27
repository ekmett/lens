{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 704
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
-- a way that they can all be composed automatically with @(.)@ from
-- Prelude.
--
-- You can derive lenses automatically for many data types:
--
-- > import Control.Lens.TH
-- > data Foo a = Foo { _fooArgs :: [String], _fooValue :: a }
-- > makeLenses ''Foo
--
-- This defines the following lenses:
--
-- > fooArgs :: Lens (Foo a) [String]
-- > fooValue :: LensFamily (Foo a) (Foo b) a b
--
----------------------------------------------------------------------------
module Control.Lens
  (
  -- * Lenses
    Lens
  , LensFamily

  -- ** Constructing Lenses
  , lens
  , iso
  , clone

  -- * Getters
  , Getter, GetterFamily
  , getting

  -- ** Getting Values
  , reading
  , readings
  , (^.), (^$)

  -- * Setters
  , Setter, SetterFamily
  , setting
  , mapped

  -- ** Setting Values
  , modifying
  , writing
  , (^%=), (^=), (^+=), (^-=), (^*=), (^/=), (^||=), (^&&=)

  -- * Manipulating State
  , access
  , (%=), (~=), (+=), (-=), (*=), (//=), (||=), (&&=)
  , (%%=)
  , Focus(..)

  -- * Folds
  , Fold
  , FoldFamily

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
  , TraversalFamily

  -- ** Common Traversals
  , traverseNothing
  , traverseValueAt
  , traverseValueAtInt
  , traverseHead, traverseTail
  , traverseLast, traverseInit
  , traverseLeft
  , traverseRight
  , traverseElement
  , TraverseByteString(..)
  , TraverseValueAtMin(..)
  , TraverseValueAtMax(..)

  -- ** Traversal Combinators
  -- , traverseOf = id
  , mapMOf
  , sequenceAOf
  , sequenceOf

  -- ** Common Lenses
  , _1
  , _2
  , valueAt
  , valueAtInt
  , contains
  , containsInt
  , identity
  , resultAt

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
import           Data.Foldable                    as Foldable
import           Data.Functor.Identity
import           Data.IntMap                      as IntMap
import           Data.IntSet                      as IntSet
import           Data.Map                         as Map
import           Data.Monoid
import           Data.Sequence                    as Seq
import           Data.Set                         as Set
import           Data.Traversable
import           Data.Word (Word8)

infixl 8 ^.
infixr 4 ^%=, ^=, ^+=, ^*=, ^-=, ^/=, ^&&=, ^||=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=
infixr 0 ^$

--------------------------
-- Lenses
--------------------------

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
--
-- Example:
--
-- > import Data.Complex
-- > imaginary :: Lens (Complex a) a
-- > imaginary f (e :+ i) = (e :+) <$> f i
--
-- > type Lens a b             = LensFamily a a b b

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
-- Despite the complicated signature the pattern for implementing a 'LensFamily' is the same as a 'Lens'.
-- in fact the implementation doesn't change, the type signature merely generalizes.
--
-- > identity :: LensFamily (Identity a) (Identity b) a b
-- > identity f (Identity a) = Identity <$> f a
type LensFamily a b c d        = forall f. Functor f => (c -> f d) -> a -> f b

--------------------------
-- Constructing Lenses
--------------------------

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

---------------
-- Getters
---------------

-- | A 'Getter' can be used directly as a 'GetterFamily' or as a 'Fold', and hence it can be as a 'FoldFamily'.
--
-- In general while your combinators may produce a 'Getter' it is better to consume any 'GetterFamily'.
--
-- > type Getter a b           = GetterFamily a a b b
type Getter a b                = forall z. (b -> Const z b) -> a -> Const z a

-- | A 'GetterFamily' describes how to retrieve a single value in a way that can be composed with
-- other lens-like constructions. It can be used directly as a 'FoldFamily', since it just
-- ignores the 'Monoid'.
type GetterFamily a b c d      = forall z. (c -> Const z d) -> a -> Const z b

-- | Build a 'Getter' or 'GetterFamily'
getting :: (a -> c) -> GetterFamily a b c d
getting f g a = Const (getConst (g (f a)))
{-# INLINE getting #-}

-------------------------------
-- Getting Values
-------------------------------

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily' or the fold of a
-- 'Fold', 'Traversal' or 'TraversalFamily' that points at monoidal
-- values.
--
-- > reading :: GetterFamily a b c d -> a -> c
reading :: ((c -> Const c d) -> a -> Const c b) -> a -> c
reading l a = getConst (l Const a)
{-# INLINE reading #-}

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily' or the fold of a
-- 'Fold', 'Traversal' or 'TraversalFamily' that points to something you want
-- to map to a monoidal value
readings :: ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
readings l f = getConst . l (Const . f)
{-# INLINE readings #-}

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

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- | Every 'Setter' can be used directly as a 'SetterFamily'.
--
-- Note: the only lens law that applies to a 'Setter' is
--
-- > writing l c (writing l b a) = writing l c a
--
-- 'reading' a 'Setter' doesn't work in general, so the other two laws can never be invoked.
--
-- > type Setter a b                = SetterFamily a a b b
type Setter a b                     = (b -> Identity b) -> a -> Identity a

-- | A 'SetterFamily' describes a way to perform polymorphic update to potentially multiple fields in a way that can be
-- composed with other lens-like constructions that can be used as a 'SetterFamily'.
--
-- The typical way to obtain a 'SetterFamily' is to build one with 'setting' or to compose some other 'Lens'-like construction
-- with a 'SetterFamily'.
--
-- Note: the only lens law that applies to a 'SetterFamily' is
--
-- > writing l c (writing l b a) = writing l c a
--
-- 'reading' a 'SetterFamily' doesn't work in general, so the other two laws can never be invoked.
type SetterFamily a b c d           = (c -> Identity d) -> a -> Identity b

-- | Build a Setter or SetterFamily
--
-- > setting . modifying = id
-- > modifying . setting = id
--
setting :: ((c -> d) -> a -> b) -> SetterFamily a b c d
setting f g a = Identity (f (runIdentity . g) a)
{-# INLINE setting #-}

-- | This setter will replace all of the values in a container.
mapped :: Functor f => SetterFamily (f a) (f b) a b
mapped = setting fmap
{-# INLINE mapped #-}

-- | Modify the target of a 'Lens', 'LensFamily' or all the targets of a
-- 'Traversal', 'TraversalFamily', 'Setter' or 'SetterFamily'
--
-- > fmap = modifying traverse
-- > setting . modifying = id
-- > modifying . setting = id
--
-- > modifying :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b

modifying :: SetterFamily a b c d -> (c -> d) -> a -> b
modifying l f a = runIdentity (l (Identity . f) a)
{-# INLINE modifying #-}

-- | Replace the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
--
-- > (<$) = writing traverse
--
-- > writing :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
writing :: SetterFamily a b c d -> d -> a -> b
writing l d a = runIdentity (l (\_ -> Identity d) a)
{-# INLINE writing #-}

-- | Modifies the target of a 'Lens', 'LensFamily', 'Setter', or 'SetterFamily'.
--
-- This is an infix version of 'modifying'
--
-- > fmap f = traverse ^%= f
--
-- > (^%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
(^%=) :: SetterFamily a b c d -> (c -> d) -> a -> b
l ^%= f = runIdentity . l (Identity . f)
{-# INLINE (^%=) #-}

-- | Replaces the target(s) of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'.
--
-- This is an infix version of 'writing'
--
-- > f <$ a = traverse ^= f $ a
--
-- > (^=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
(^=) :: SetterFamily a b c d -> d -> a -> b
l ^= v = runIdentity . l (Identity . const v)
{-# INLINE (^=) #-}

-- | Increment the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> _1 ^+= 1 $ (1,2)
-- > (2,2)
--
-- > (^+=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
(^+=) :: Num c => Setter a c -> c -> a -> a
l ^+= n = modifying l (+ n)
{-# INLINE (^+=) #-}

-- | Multiply the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> _2 ^*= 4 $ (1,2)
-- > (1,8)
--
-- > (^*=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
(^*=) :: Num c => Setter a c -> c -> a -> a
l ^*= n = modifying l (* n)
{-# INLINE (^*=) #-}

-- | Decrement the target(s) of a numerically valued 'Lens' or 'Setter'
--
-- > ghci> _1 ^-= 2 $ (1,2)
-- > (-1,2)
--
-- > (^-=) :: ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
(^-=) :: Num c => Setter a c -> c -> a -> a
l ^-= n = modifying l (subtract n)
{-# INLINE (^-=) #-}

-- | Divide the target(s) of a numerically valued 'Setter'
--
-- > (^/=) :: Fractional c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
(^/=) :: Fractional b => Setter a b -> b -> a -> a
l ^/= n = modifying l (/ n)

-- | Logically '||' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- > (^||=):: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
(^||=):: Setter a Bool -> Bool -> a -> a
l ^||= n = modifying l (|| n)
{-# INLINE (^||=) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
-- (^&&=) :: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
(^&&=) :: Setter a Bool -> Bool -> a -> a
l ^&&= n = modifying l (&& n)
{-# INLINE (^&&=) #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | This is a lens family that can change the value (and type) of the first field of
-- a pair.
--
-- > ghci> (1,2)^._1
-- > 1
--
-- > ghci> _1 ^= "hello" $ (1,2)
-- > ("hello",2)
--
_1 :: LensFamily (a,c) (b,c) a b
_1 f (a,c) = (\b -> (b,c)) <$> f a
{-# INLINE _1 #-}

-- | As '_1', but for the second field of a pair.
--
-- > anyOf _2 :: (c -> Bool) -> (a, c) -> Bool
-- > traverse._2 :: (Applicative f, Traversable t) => (a -> f b) -> t (c, a) -> f (t (c, b))
-- > foldMapOf (traverse._2) :: (Traversable t, Monoid m) => (c -> m) -> t (b, c) -> m
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

-- | Modify the value of a field in our monadic state and return some information about it
(%%=) :: MonadState a m => ((b -> (c,b)) -> a -> (c,a)) -> (b -> (c, b)) -> m c
l %%= f = state (l f)
{-# INLINE (%%=) #-}

-- | Set the value of a field in our monadic state
(~=) :: MonadState a m => Setter a b -> b -> m ()
l ~= b = modify $ l ^= b
{-# INLINE (~=) #-}

-- | Modify the value of a field in our monadic state
(%=) :: MonadState a m => Setter a b -> (b -> b) -> m ()
l %= f = modify $ l ^%= f
{-# INLINE (%=) #-}

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
-- Folds
--------------------------
-- | Every 'Fold' can be used directly as a 'FoldFamily' (and you should probably be using a 'FoldFamily'
-- instead.)
--
-- > type Fold a b           = FoldFamily a b c d
type Fold a b                = forall m. Monoid m => (b -> Const m b)-> a -> Const m a

-- | A 'FoldFamily' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A @'FoldFamily' a b c d@ provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other 'FoldFamily' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f c)@, then there should be a
-- 'fooOf' method that takes a @'FoldFamily' a b c d@ and a value of type @a@.
--
type FoldFamily a b c d      = forall m. Monoid m => (c -> Const m d) -> a -> Const m b

-- | Obtain a 'FoldFamily' from any 'Foldable'
folded :: Foldable f => FoldFamily (f c) b c d
folded = folding id
{-# INLINE folded #-}

-- | Building a FoldFamily
folding :: Foldable f => (a -> f c) -> FoldFamily a b c d
folding f g a = Const (foldMap (getConst . g) (f a))
{-# INLINE folding #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- |
-- > foldMap = foldMapOf folded
--
-- > foldMapOf = readings
--
-- > foldMapOf :: GetterFamily a b c d -> (c -> m) -> a -> m
-- > foldMapOf :: Monoid m => FoldFamily a b c d -> (c -> m) -> a -> m
foldMapOf :: ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

-- |
-- > fold = foldOf folded
--
-- > foldOf = reading
--
-- > foldOf :: GetterFamily a b m d -> a -> m
-- > foldOf :: Monoid m => FoldFamily a b m d -> a -> m
foldOf :: ((m -> Const m d) -> a -> Const m b) -> a -> m
foldOf l = getConst . l Const
{-# INLINE foldOf #-}

-- |
-- > foldr = foldrOf folded
--
-- > foldrOf :: GetterFamily a b c d -> (c -> e -> e) -> e -> a -> e
-- > foldrOf :: FoldFamily a b c d -> (c -> e -> e) -> e -> a -> e
foldrOf :: ((c -> Const (Endo e) d) -> a -> Const (Endo e) b) -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z
{-# INLINE foldrOf #-}

-- |
-- > toList = toListOf folded
--
-- > toListOf :: GetterFamily a b c d -> a -> [c]
-- > toListOf :: FoldFamily a b c d -> a -> [c]
toListOf :: ((c -> Const [c] d) -> a -> Const [c] b) -> a -> [c]
toListOf l = foldMapOf l return
{-# INLINE toListOf #-}

-- |
-- > and = andOf folded
--
-- > andOf :: GetterFamily a b Bool d -> a -> Bool
-- > andOf :: FoldFamily a b Bool d -> a -> Bool
andOf :: ((Bool -> Const All d) -> a -> Const All b) -> a -> Bool
andOf l = getAll . foldMapOf l All
{-# INLINE andOf #-}

-- |
-- > or = orOf folded
--
-- > orOf :: GetterFamily a b Bool d -> a -> Bool
-- > orOf :: FoldFamily a b Bool d -> a -> Bool
orOf :: ((Bool -> Const Any d) -> a -> Const Any b) -> a -> Bool
orOf l = getAny . foldMapOf l Any
{-# INLINE orOf #-}

-- |
-- > any = anyOf folded
--
-- > anyOf :: GetterFamily a b c d -> (c -> Bool) -> a -> Bool
-- > anyOf :: FoldFamily a b c d -> (c -> Bool) -> a -> Bool
anyOf :: ((c -> Const Any d) -> a -> Const Any b) -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)
{-# INLINE anyOf #-}

-- |
-- > all = allOf folded
--
-- > allOf :: GetterFamily a b c d -> (c -> Bool) -> a -> Bool
-- > allOf :: FoldFamily a b c d -> (c -> Bool) -> a -> Bool
allOf :: ((c -> Const All d) -> a -> Const All b) -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)
{-# INLINE allOf #-}

-- |
-- > product = productOf folded
--
-- > productOf :: GetterFamily a b c d -> a -> c
-- > productOf :: Num c => FoldFamily a b c d -> a -> c
productOf :: ((c -> Const (Product c) d) -> a -> Const (Product c) b) -> a -> c
productOf l = getProduct . foldMapOf l Product
{-# INLINE productOf #-}

-- |
-- > sum = sumOf folded
--
-- > sumOf _1 :: (a, b) -> a
-- > sumOf (folded._1) :: (Foldable f, Num a) => f (a, b) -> a
--
-- > sumOf :: GetterFamily a b c d -> a -> c
-- > sumOf :: Num c => FoldFamily a b c d -> a -> c
sumOf ::  ((c -> Const (Sum c) d) -> a -> Const (Sum c) b) -> a -> c
sumOf l = getSum . foldMapOf l Sum
{-# INLINE sumOf #-}

-- |
--
-- When passed a 'Getter', 'traverseOf_' can work over a 'Functor'.
--
-- When passed a 'FoldFamily', 'traverseOf_' requires an 'Applicative'.
--
-- > traverse_ = traverseOf_ folded

-- > traverseOf_ _2 :: Functor f => (c -> f e) -> (c1, c) -> f ()
-- > traverseOf_ traverseLeft :: Applicative f => (a -> f b) -> Either a c -> f ()
--
-- The rather specific signature of traverseOf_ allows it to be used as if the signature was either:
--
-- > traverseOf_ :: Functor f => GetterFamily a b c d -> (c -> f e) -> a -> f ()
-- > traverseOf_ :: Applicative f => FoldFamily a b c d -> (c -> f e) -> a -> f ()
traverseOf_ :: Functor f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversed . foldMapOf l (Traversed . (() <$) . f)
{-# INLINE traverseOf_ #-}

-- |
-- > for_ = forOf_ folded
--
-- > forOf_ :: Functor f => GetterFamily a b c d -> a -> (c -> f e) -> f ()
-- > forOf_ :: Applicative f => FoldFamily a b c d -> a -> (c -> f e) -> f ()
forOf_ :: Functor f => ((c -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> (c -> f e) -> f ()
forOf_ l a f = traverseOf_ l f a
{-# INLINE forOf_ #-}

-- |
-- > sequenceA_ = sequenceAOf_ folded
--
-- > sequenceAOf_ :: Functor f => GetterFamily a b (f ()) d -> a -> f ()
-- > sequenceAOf_ :: Applicative f => FoldFamily a b (f ()) d -> a -> f ()
sequenceAOf_ :: Functor f => ((f () -> Const (Traversed f) d) -> a -> Const (Traversed f) b) -> a -> f ()
sequenceAOf_ l = getTraversed . foldMapOf l (Traversed . (() <$))
{-# INLINE sequenceAOf_ #-}

-- |
-- > mapM_ = mapMOf_ folded
--
-- > mapMOf_ :: Monad m => GetterFamily a b c d -> (c -> m e) -> a -> m ()
-- > mapMOf_ :: Monad m => FoldFamily a b c d -> (c -> m e) -> a -> m ()
mapMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> (c -> m e) -> a -> m ()
mapMOf_ l f = unwrapMonad . traverseOf_ l (WrapMonad . f)
{-# INLINE mapMOf_ #-}

-- |
-- > forM_ = forMOf_ folded
--
-- > forMOf_ :: Monad m => GetterFamily a b c d -> a -> (c -> m e) -> m ()
-- > forMOf_ :: Monad m => FoldFamily a b c d -> a -> (c -> m e) -> m ()
forMOf_ :: Monad m => ((c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> (c -> m e) -> m ()
forMOf_ l a f = mapMOf_ l f a
{-# INLINE forMOf_ #-}

-- |
-- > sequence_ = sequenceOf_ folded
--
-- > sequenceOf_ :: Monad m => GetterFamily a b (m b) d -> a -> m ()
-- > sequenceOf_ :: Monad m => FoldFamily a b (m b) d -> a -> m ()
sequenceOf_ :: Monad m => ((m c -> Const (Traversed (WrappedMonad m)) d) -> a -> Const (Traversed (WrappedMonad m)) b) -> a -> m ()
sequenceOf_ l = unwrapMonad . traverseOf_ l WrapMonad
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > asum = asumOf folded
--
-- > asumOf :: Alternative f => GetterFamily a b c d -> a -> f c
-- > asumOf :: Alternative f => FoldFamily a b c d -> a -> f c
asumOf :: Alternative f => ((f c -> Const (Endo (f c)) d) -> a -> Const (Endo (f c)) b) -> a -> f c
asumOf l = foldrOf l (<|>) Applicative.empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- > msum = msumOf folded
--
-- > msumOf :: MonadPlus m => GetterFamily a b c d -> a -> m c
-- > msumOf :: MonadPlus m => FoldFamily a b c d -> a -> m c
msumOf :: MonadPlus m => ((m c -> Const (Endo (m c)) d) -> a -> Const (Endo (m c)) b) -> a -> m c
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- |
-- > elem = elemOf folded
--
-- > elemOf :: Eq c => GetterFamily a b c d -> c -> a -> Bool
-- > elemOf :: Eq c => FoldFamily a b c d -> c -> a -> Bool
elemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- |
-- > notElem = notElemOf folded
--
-- > notElemOf :: Eq c => GetterFamily a b c d -> c -> a -> Bool
-- > notElemOf :: Eq c => FoldFamily a b c d -> c -> a -> Bool
notElemOf :: Eq c => ((c -> Const Any d) -> a -> Const Any b) -> c -> a -> Bool
notElemOf l c = not . elemOf l c
{-# INLINE notElemOf #-}

-- |
-- > concatMap = concatMapOf folded
--
-- > concatMapOf :: GetterFamily a b c d -> (c -> [e]) -> a -> [e]
-- > concatMapOf :: FoldFamily a b c d -> (c -> [e]) -> a -> [e]
concatMapOf :: ((c -> Const [e] d) -> a -> Const [e] b) -> (c -> [e]) -> a -> [e]
concatMapOf l ces a = getConst  (l (Const . ces) a)
{-# INLINE concatMapOf #-}

-- |
-- > concat = concatOf folded
--
-- > concatOf :: GetterFamily a b [e] d -> a -> [e]
-- > concatOf :: FoldFamily a b [e] d -> a -> [e]
concatOf :: (([e] -> Const [e] d) -> a -> Const [e] b) -> a -> [e]
concatOf = reading
{-# INLINE concatOf #-}

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | Every 'Traversal' can be used as a 'TraversalFamily' or a 'Setter' or 'Fold', so it can transitively be used as a
-- 'FoldFamily' or 'SetterFamily' as well.
--
-- > type Traversal a b             = TraversalFamily a a b b
type Traversal a b                  = forall f. Applicative f => (b -> f b) -> a -> f a


-- | A 'TraversalFamily' can be used directly as a 'SetterFamily' or a 'FoldFamily' and provides
-- the ability to both read and update multiple fields, subject to the (relatively weak) 'TraversalFamily' laws.
--
-- These are also known as @MultiLens@ families, but they have the signature and spirit of
--
-- > traverse :: Traversable f => TraversalFamiy (f a) (f b) a b
--
-- and the more evocative name suggests their application.
type TraversalFamily a b c d        = forall f. Applicative f => (c -> f d) -> a -> f b

--------------------------
-- Traversal combinators
--------------------------

-- |
-- > mapM = mapMOf traverse
--
-- > mapMOf :: Monad m => LensFamily a b c d -> (c -> m d) -> a -> m b
-- > mapMOf :: Monad m => TraversalFamily a b c d -> (c -> m d) -> a -> m b
mapMOf :: ((c -> WrappedMonad m d) -> a -> WrappedMonad m b) -> (c -> m d) -> a -> m b
mapMOf l cmd a = unwrapMonad (l (WrapMonad . cmd) a)
{-# INLINE mapMOf #-}

-- |
-- > sequenceA = sequenceAOf traverse
--
-- > sequenceAOf :: Applicative f => LensFamily a b (f c) (f c) -> a -> f b
-- > sequenceAOf :: Applicative f => TraversalFamily a b (f c) (f c) -> a -> f b
sequenceAOf :: Applicative f => ((f c -> f (f c)) -> a -> f b) -> a -> f b
sequenceAOf l = l pure
{-# INLINE sequenceAOf #-}

-- |
-- > sequence = sequenceOf traverse
--
-- > sequenceOf :: Monad m => LensFamily a b (m c) (m c) -> a -> m b
-- > sequenceOf :: Monad m => TraversalFamily a b (m c) (m c) -> a -> m b
sequenceOf :: Monad m => ((m c -> WrappedMonad m (m c)) -> a -> WrappedMonad m b) -> a -> m b
sequenceOf l = unwrapMonad . l pure
{-# INLINE sequenceOf #-}

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

--
-- | > traverseTail :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
traverseTail :: Traversal [a] [a]
traverseTail _ [] = pure []
traverseTail f (a:as) = (a:) <$> f as
{-# INLINE traverseTail #-}

traverseLast :: Traversal [a] a
traverseLast _ []     = pure []
traverseLast f [a]    = return <$> f a
traverseLast f (a:as) = (a:) <$> traverseLast f as
{-# INLINE traverseLast #-}

-- The traversal for reading and writing to the tail of a list

-- | Traverse all but the last element of a list
--
-- > traverseInit :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
traverseInit :: Traversal [a] [a]
traverseInit _ [] = pure []
traverseInit f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE traverseInit #-}

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
-- > traverseValueAt :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
-- > traverseValueAt k = valueAt k . traverse
traverseValueAt :: Ord k => k -> Traversal (Map k v) v
traverseValueAt k = valueAt k . traverse
{-# INLINE traverseValueAt #-}

-- | Traverse the value at a given key in an IntMap
--
-- > traverseValueAtInt :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
-- > traverseValueAtInt k = valueAtInt k . traverse
traverseValueAtInt :: Int -> Traversal (IntMap v) v
traverseValueAtInt k = valueAtInt k . traverse
{-# INLINE traverseValueAtInt #-}

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
  -- > anyOf traverseByteString (==0x80) :: TraverseByteString b => b -> Bool
  traverseByteString :: Traversal t Word8

instance TraverseByteString Strict.ByteString where
  traverseByteString f = fmap Strict.pack . traverse f . Strict.unpack

instance TraverseByteString Lazy.ByteString where
  traverseByteString f = fmap Lazy.pack . traverse f . Lazy.unpack

class TraverseValueAtMin t where
  traverseValueAtMin :: Traversal (t v) v
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

class TraverseValueAtMax t where
  traverseValueAtMax :: Traversal (t v) v

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

------------------------------------------------------------------------------
-- Cloning Lenses
------------------------------------------------------------------------------

-- | Cloning a 'Lens' or 'LensFamily' is one way to make sure you arent given
-- something weaker, such as a 'Traversal' or 'TraversalFamily', and can be used
-- as a way to pass around lenses that have to be monomorphic in 'f'.
clone :: Functor f => 
   ((c -> IndexedStore c d d) -> a -> IndexedStore c d b) ->
  (c -> f d) -> a -> f b
clone f cfd a = case f (IndexedStore id) a of
  IndexedStore db c -> db <$> cfd c
{-# INLINE clone #-}

------------------------------------------------------------------------------
-- Implementation details
------------------------------------------------------------------------------

data IndexedStore c d a = IndexedStore (d -> a) c

instance Functor (IndexedStore c d) where
  fmap f (IndexedStore g c) = IndexedStore (f . g) c

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
