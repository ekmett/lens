{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens
-- Copyright   :  (C) 2012 Edward Kmett
--                (C) 2012 Dan Burton
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This package provides lenses that are compatible with other van
-- Laarhoven lens libraries, while reducing the complexty of the imports.
--
-- Lenses produced by this library are compatible with other van Laarhoven
-- lens family libraries, such as lens-family, lens-family-core and
-- lens-family-th, but the API is simpler.
--
-- Note: If you merely want your library to provide lenses you may not have
-- to actually import _any_ lens library, for a "Lens Bar Foo", just export
-- a function with the signature:
--
-- > foo :: Functor f => (Foo -> f Foo) -> Bar -> f Bar
--
-- and then you can compose it with other lenses using (.).
--
-- This package provides lenses, lens families, setters, setter families,
-- getters, multilenses, multi-getters, and multi-lens families in such
-- a way that they can all be composed automatically with (.).
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

  -- * Constructing lenses
  , makeLenses
  , makeLensesBy
  , makeLensesFor
  , lens
  , iso
  , clone
  , getting
 -- , gettingMany
  , setting

  -- * Reading
  , getLens, mapOf, setLens
  , (^.), (^$)
  , (^%=), (^=), (^+=), (^-=), (^*=), (^/=), (^||=), (^&&=)

  -- * Manipulating State
  , access
  , Focus(..)
  , (%=), (~=), (%%=), (+=), (-=), (*=), (//=), (||=), (&&=)

  -- * Common lenses
  , fstL
  , sndL
  , keyL
  , intKeyL
  , memberL
  , intMemberL
  , identityL
  , atL

  -- * MultiGetters

  -- ** MultiGetter combinators
  , foldMapOf
  , toListOf
  , anyOf, allOf
  , traverseOf_
  , forOf_
  , sequenceAOf_
  , mapMOf_
  , forMOf_
  , sequenceOf_
  , traverseOf
  , mapMOf
  , sequenceAOf
  , sequenceOf

  -- ** multilenses and multigetters
--  , folded
  , constML
  , mapML
  , intMapML
  , headML
  , tailML
  , leftML
  , elementML

  -- * Implementation details
  , IndexedStore(..)
  , Focusing(..)
  ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State.Lazy   as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Reader
import           Data.Char (toLower)
import           Data.Functor.Identity
import           Data.IntMap                      as IntMap
import           Data.IntSet                      as IntSet
import           Data.Map                         as Map
import           Data.Monoid (Monoid(..), Any(..), All(..), Endo(..))
import           Data.Set                         as Set
import           Data.Traversable
import           Language.Haskell.TH

infixl 8 ^.
infixr 4 ^%=, ^=, ^+=, ^*=, ^-=, ^/=, ^&&=, ^||=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=
infixr 0 ^$

type Lens a b                = forall f. Functor f => (b -> f b) -> a -> f a
type LensFamily a b c d      = forall f. Functor f => (c -> f d) -> a -> f b
type Getter a b              = forall x y z. (b -> Const z x) -> a -> Const z y
type Setter a b              = (b -> Identity b) -> a -> Identity a
type SetterFamily a b c d    = (c -> Identity d) -> a -> Identity b
type MultiGetter a c         = forall x y m. Monoid m => (c -> Const m x) -> a -> Const m y
type MultiLens a b           = forall f. Applicative f => (b -> f b) -> a -> f a
type MultiLensFamily a b c d = forall f. Applicative f => (c -> f d) -> a -> f b

-- | Build a lens from a getter and a setter
lens :: Functor f => (a -> c) -> (d -> a -> b) -> (c -> f d) -> a -> f b
lens ac dab cfd a = (`dab` a) <$> cfd (ac a)
{-# INLINE lens #-}

-- | Built a lens from an isomorphism or an isomorphism family
iso :: Functor f => (a -> c) -> (d -> b) -> (c -> f d) -> a -> f b
iso f g h a = g <$> h (f a )
{-# INLINE iso #-}

-- | Build a getter
getting :: (a -> b) -> Getter a b
getting f g a = Const (getConst (g (f a)))
{-# INLINE getting #-}

-- | Building a multigetter
-- gettingMany :: Foldable f => (a -> f b) -> (f b -> Const m x) -> a -> Const m y
-- gettingMany :: Foldable f => (a -> f b) -> MultiGetter a b
-- gettingMany f g a = Const (getConst (foldMap g (f a)))

-- | Build a setter
setting :: ((c -> d) -> a -> b) -> SetterFamily a b c d
setting f g a = Identity (f (runIdentity . g) a)
{-# INLINE setting #-}

------------------------------------------------------------------------------
-- Using Lenses
------------------------------------------------------------------------------

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily' or the fold of a
-- 'MultiGetter', 'MultiLens' or 'MultiLensFamily' that points at monoidal
-- values.
getLens :: ((c -> Const c d) -> a -> Const c b) -> a -> c
getLens l a = getConst (l Const a)
{-# INLINE getLens #-}

-- | Modify the target of a 'Lens', 'LensFamily' or all the targets of a
-- 'Multilens', 'MultiLensFamily', 'Setter' or 'SetterFamily'
mapOf :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
mapOf l f a = runIdentity (l (Identity . f) a)
{-# INLINE mapOf #-}

-- | Replace the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
setLens :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
setLens l d a = runIdentity (l (\_ -> Identity d) a)
{-# INLINE setLens #-}

-- | Read the value of a 'Getter', 'Lens' or 'LensFamily'.
-- This is the same operation as 'getLens'.
(^$) :: ((c -> Const c d) -> a -> Const c b) -> a -> c
l ^$ a = getConst (l Const a)
{-# INLINE (^$) #-}

-- | Read a field from a 'Getter', 'Lens' or 'LensFamily'.
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with (Prelude..) This is the same operation as 'flip getLens'
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
-- This is an infix version of 'setLens'
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

instance Focus Lazy.StateT where
  focus l (Lazy.StateT m) = Lazy.StateT $ \a -> unfocusing (l (Focusing . m) a)

-- | We can focus Reader environments, too!
instance Focus ReaderT where
  focus l (ReaderT m) = ReaderT $ \a -> liftM undefined $  unfocusing $ l (\b -> Focusing $ (\c -> (c,b)) `liftM` m b) a

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

-- | > foldMapOf :: Monoid m => MultiGetter a b -> (b -> m) -> a -> m
foldMapOf :: Monoid m => ((c -> Const m d) -> a -> Const m b) -> (c -> m) -> a -> m
foldMapOf l f = getConst . l (Const . f)

-- | > foldOf :: Monoid m => MultiGetter a m -> a -> m
foldOf :: Monoid m => ((m -> Const m n) -> a -> Const m b) -> a -> m
foldOf l = getConst . l Const

-- | > foldrOf :: MultiGetter a b -> (b -> c -> c) -> c -> a -> c
foldrOf :: ((c -> Const (Endo e) d) -> a -> Const (Endo e) b) -> (c -> e -> e) -> e -> a -> e
foldrOf l f z t = appEndo (foldMapOf l (Endo . f) t) z

-- | > toListOf :: MultiGetter a b -> a -> [b]
toListOf :: ((c -> Const [c] d) -> a -> Const [c] b) -> a -> [c]
toListOf l = foldMapOf l return

-- | > anyOf :: MultiGetter a b -> (b -> Bool) -> a -> Bool
anyOf :: ((c -> Const Any d) -> a -> Const Any b) -> (c -> Bool) -> a -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)

-- | > allOf :: MultiGetter a b -> (b -> Bool) -> a -> Bool
allOf :: ((c -> Const All d) -> a -> Const All b) -> (c -> Bool) -> a -> Bool
allOf l f = getAll . foldMapOf l (All . f)

-- | > traverseOf_ :: Applicative f => MultiGetter a b -> (b -> f c) -> a -> f ()
traverseOf_ :: Applicative f => ((c -> Const (Traversal f) d) -> a -> Const (Traversal f) b) -> (c -> f e) -> a -> f ()
traverseOf_ l f = getTraversal . foldMapOf l (Traversal . (() <$) . f)
{-# INLINE traverseOf_ #-}

-- | > forOf_ :: Applicative f => MultiGetter a b -> a -> (b -> f c) -> f ()
forOf_ :: Applicative f => ((c -> Const (Traversal f) d) -> a -> Const (Traversal f) b) -> a -> (c -> f e) -> f ()
forOf_ l a f = traverseOf_ l f a
{-# INLINE forOf_ #-}

-- | > sequenceAOf_ :: Applicative f => MultiGetter a (f ()) -> a -> f ()
sequenceAOf_ :: Applicative f => ((f () -> Const (Traversal f) d) -> a -> Const (Traversal f) e) -> a -> f ()
sequenceAOf_ l = getTraversal . foldMapOf l (Traversal . (() <$))
{-# INLINE sequenceAOf_ #-}

-- | > mapMOf_ :: Monad m => MultiGetter a b -> (b -> m c) -> a -> m ()
mapMOf_ :: Monad m => ((c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> (c -> m e) -> a -> m ()
mapMOf_ l f = unwrapMonad . traverseOf_ l (WrapMonad . f)
{-# INLINE mapMOf_ #-}

-- | > forMOf_ :: Monad m => MultiGetter a b -> a -> (b -> m c) -> m ()
forMOf_ :: Monad m => ((c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> a -> (c -> m e) -> m ()
forMOf_ l a f = mapMOf_ l f a
{-# INLINE forMOf_ #-}

-- | > sequenceOf_ :: Monad m => MultiGetter a (m b) -> a -> m ()
sequenceOf_ :: Monad m => ((m c -> Const (Traversal (WrappedMonad m)) d) -> a -> Const (Traversal (WrappedMonad m)) b) -> a -> m ()
sequenceOf_ l = unwrapMonad . traverseOf_ l WrapMonad
{-# INLINE sequenceOf_ #-}

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

-- folded :: Foldable f => MultiGetter (f a) a
-- folded = gettingMany id

--------------------------
-- Multilenses
--------------------------

-- | This is the partial lens that never succeeds are returning any values
constML :: Applicative f => (a -> f a) -> b -> f b
constML = const pure

headML :: Applicative f => (a -> f a) -> [a] -> f [a]
headML _ [] = pure []
headML f (a:as) = (:as) <$> f a
{-# INLINE headML #-}

tailML :: Applicative f => ([a] -> f [a]) -> [a] -> f [a]
tailML _ [] = pure []
tailML f (a:as) = (a:) <$> f as
{-# INLINE tailML #-}

leftML :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
leftML f (Left a)  = Left <$> f a
leftML _ (Right c) = pure $ Right c
{-# INLINE leftML #-}

mapML :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
mapML k f m = case Map.lookup k m of
  Nothing -> pure m
  Just v -> (\v' -> Map.insert k v' m) <$> f v
{-# INLINE mapML #-}

intMapML :: Applicative f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
intMapML k f m = case IntMap.lookup k m of
  Nothing -> pure m
  Just v -> (\v' -> IntMap.insert k v' m) <$> f v
{-# INLINE intMapML #-}

elementML :: (Applicative f, Traversable t) => Int -> (a -> f a) -> t a -> f (t a)
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
