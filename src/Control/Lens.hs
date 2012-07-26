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
-- A self-contained lens library with lenses that are compatible with other
-- van Laarhoven lens libraries.
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
----------------------------------------------------------------------------
module Control.Lens
  (
  -- * Lenses
    Lens
  , LensFamily

  -- * Constructing lenses
  , makeLenses
  , makeLensesBy
  , makeLensesFor
  , lens
  , iso
  , clone

  -- * Reading from lenses
  , getL
  , modL
  , setL
  , (^.), (^$)
  , (^%=), (^=), (^+=), (^-=), (^*=), (^/=), (^||=), (^&&=)

  -- * Manipulating state
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
  , funL

  -- ** Getters
  , Getter
  , getting

  -- ** Setters
  , Setter
  , SetterFamily
  , setting

  -- * Implementation details
  , IndexedStore(..)
  , Focusing(..)
  ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Reader
import           Data.Char (toLower)
import           Data.Functor.Identity
import           Data.IntMap as IntMap
import           Data.IntSet as IntSet
import           Data.Map as Map
import           Data.Set as Set
import           Language.Haskell.TH

infixl 8 ^.
infixr 4 ^%=, ^=, ^+=, ^*=, ^-=, ^/=, ^&&=, ^||=
infix  4 ~=, %=, %%=, +=, -=, *=, //=, &&=, ||=
infixr 0 ^$

type Lens a b                 = forall f. Functor f => (b -> f b) -> a -> f a
type LensFamily a b c d       = forall f. Functor f => (c -> f d) -> a -> f b
type Getter a b               = forall x y z. (b -> Const z x) -> a -> Const z y
type Setter a b               = (b -> Identity b) -> a -> Identity a
type SetterFamily a b c d     = (c -> Identity d) -> a -> Identity b

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

-- | Build a setter
setting :: ((c -> d) -> a -> b) -> SetterFamily a b c d
setting f g a = Identity (f (runIdentity . g) a)
{-# INLINE setting #-}

------------------------------------------------------------------------------
-- Using Lenses
------------------------------------------------------------------------------

-- | Get the value of a 'Getter', 'Lens' or 'LensFamily'
getL :: ((c -> Const c d) -> a -> Const c b) -> a -> c
getL l a = getConst (l Const a)
{-# INLINE getL #-}

-- | Modify the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
modL :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
modL l f a = runIdentity (l (Identity . f) a)
{-# INLINE modL #-}

-- | Replace the target of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'
setL :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
setL l d a = runIdentity (l (\_ -> Identity d) a)
{-# INLINE setL #-}

-- | Read the value of a 'Getter', 'Lens' or 'LensFamily'.
-- This is the same operation as 'getL'.
(^$) :: ((c -> Const c d) -> a -> Const c b) -> a -> c
l ^$ a = getConst (l Const a)
{-# INLINE (^$) #-}

-- | Read a field from a 'Getter', 'Lens' or 'LensFamily'.
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with (Prelude..) This is the same operation as flip getL
--
-- > ghci> ((0, 1 :+ 2), 3)^.fstL.sndL.getting magnitude
-- > 2.23606797749979
(^.) :: a -> ((c -> Const c d) -> a -> Const c b) -> c
a ^. l = getConst (l Const a)
{-# INLINE (^.) #-}

-- | Modifies the target of a 'Lens', 'LensFamily', 'Setter', or 'SetterFamily'.
--
-- This is an infix version of 'modL'
(^%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
l ^%= f = runIdentity . l (Identity . f)
{-# INLINE (^%=) #-}

-- | Replaces the target(s) of a 'Lens', 'LensFamily', 'Setter' or 'SetterFamily'.
--
-- This is an infix version of 'setL'
(^=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
l ^= v = runIdentity . l (Identity . const v)
{-# INLINE (^=) #-}

-- | Increment the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> fstL ^+= 1 $ (1,2)
-- > (2,2)
(^+=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^+= n = modL l (+ n)
{-# INLINE (^+=) #-}

-- | Multiply the target(s) of a numerically valued 'Lens' or Setter'
--
-- > ghci> sndL ^*= 4 $ (1,2) 
-- > (1,8)
(^*=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^-= n = modL l (`subtract` n)
{-# INLINE (^-=) #-}

-- | Decrement the target(s) of a numerically valued 'Lens' or 'Setter'
--
-- > ghci> fstL ^-= 2 $ (1,2)
-- > (-1,2)
(^-=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^*= n = modL l (* n)
{-# INLINE (^*=) #-}

-- | Divide the target(s) of a numerically valued 'Lens' or 'Setter'
(^/=) :: Fractional c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l ^/= n = modL l (/ n)

-- | Logically '||' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(^||=):: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
l ^||= n = modL l (|| n)
{-# INLINE (^||=) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(^&&=) :: ((Bool -> Identity Bool) -> a -> Identity a) -> Bool -> a -> a
l ^&&= n = modL l (&& n)
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
-- > ghci> Map.fromList [("hello",12)]  ^. keyL "hello"
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
identityL :: Functor f => (a -> f b) -> Identity a -> f (Identity b)
identityL f (Identity a) = Identity <$> f a
{-# INLINE identityL #-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
funL :: (Functor f, Eq e) => e -> (a -> f a) -> (e -> a) -> f (e -> a)
funL e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE funL #-}

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

class Focus st where
  -- | Use a lens to lift an operation with simpler state into a larger context
  focus :: Monad m => ((b -> Focusing m c b) -> a -> Focusing m c a) -> st b m c -> st a m c

instance Focus Strict.StateT where
  focus l (Strict.StateT m) = Strict.StateT $ \a -> unfocusing (l (Focusing . m) a)

instance Focus Lazy.StateT where
  focus l (Lazy.StateT m) = Lazy.StateT $ \a -> unfocusing (l (Focusing . m) a)

-- | We can focus Reader environments, too!
instance Focus ReaderT where
  focus l (ReaderT m) = ReaderT $ \a -> liftM undefined $  unfocusing $ l (\b -> Focusing $ (\c -> (c,b)) `liftM` m b) a

(~=) :: MonadState a m => Setter a b -> b -> m ()
l ~= b = modify (l ^= b)
{-# INLINE (~=) #-}

(%=) :: MonadState a m => Setter a b -> (b -> b) -> m ()
l %= f = modify (l ^%= f)
{-# INLINE (%=) #-}

(%%=) :: MonadState a m => ((b -> (c,b)) -> a -> (c,a)) -> (b -> (c, b)) -> m c
l %%= f = state (l f)
{-# INLINE (%%=) #-}

(+=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l += b = modify $ l ^+= b
{-# INLINE (+=) #-}

(-=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l -= b = modify $ l ^-= b
{-# INLINE (-=) #-}

(*=) :: (MonadState a m, Num b) => Setter a b -> b -> m ()
l *= b = modify $ l ^*= b
{-# INLINE (*=) #-}

(//=) ::  (MonadState a m, Fractional b) => Setter a b -> b -> m ()
l //= b = modify $ l ^/= b
{-# INLINE (//=) #-}

(&&=):: MonadState a m => Setter a Bool -> Bool -> m ()
l &&= b = modify $ l ^&&= b
{-# INLINE (&&=) #-}

(||=) :: MonadState a m => Setter a Bool -> Bool -> m ()
l ||= b = modify $ l ^||= b
{-# INLINE (||=) #-}

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
  concat <$> mapM derive1 constructorFields

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
