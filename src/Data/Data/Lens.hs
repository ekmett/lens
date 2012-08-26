{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Data.Lens
-- Copyright   :  (C) 2012 Edward Kmett, (C) 2006-2012 Neil Mitchell
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- Smart and naïve generic traversals given 'Data' instances.
--
-- 'every', 'uniplate', and 'biplate' each build up information about what
-- types can be contained within another type to speed up 'Traversal'.
--
----------------------------------------------------------------------------
module Data.Data.Lens
  ( every
  , tinplate
  , uniplate
  , biplate
  -- * Traversal of Data
  , gtraverse
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Exception as E
import           Control.Lens
import           Data.Data
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashSet as S
import           Data.HashSet (HashSet)
import           Data.IORef
import           Data.Monoid
import           GHC.IO
import           GHC.Exts (realWorld#)
import           Unsafe.Coerce as Unsafe

-------------------------------------------------------------------------------
-- Generic Traversal
-------------------------------------------------------------------------------

-- | A generic applicative transformation that maps over the immediate subterms.
--
-- 'gtraverse' is to 'traverse' what 'gmapM' is to 'mapM'
--
-- This really belongs in @Data.Data@.
gtraverse :: (Applicative f, Data a) => (forall d. Data d => d -> f d) -> a -> f a
gtraverse f = gfoldl (\x y -> x <*> f y) pure

-------------------------------------------------------------------------------
-- Naïve Traversal
-------------------------------------------------------------------------------

-- | Naïve 'Traversal' using 'Data'. This does not attempt to optimize the traversal.
--
-- This is primarily useful when the children are immediately obvious, and for benchmarking.
--
-- @
-- 'tinplate' :: ('Data' a, 'Typeable' b) => 'Simple' 'Traversal' a b
-- @
tinplate :: (Data a, Typeable b) => Simple Traversal a b
tinplate f = gfoldl (step f) pure
{-# INLINE tinplate #-}

step :: (Applicative f, Typeable b, Data d) => (b -> f b) -> f (d -> e) -> d -> f e
step f w d = w <*> case cast d of
  Just b  -> unsafeCoerce <$> f b
  Nothing -> tinplate f d
{-# INLINE step #-}

-------------------------------------------------------------------------------
-- Smart Traversal
-------------------------------------------------------------------------------

-- | Find every occurence of a given type @b@ recursively that doesn't require passing through something of type @b@
-- using 'Data', while avoiding traversal of areas that cannot contain a value of type @b@.
every :: forall a b. (Data a, Typeable b) => Simple Traversal a b
every f a = uniplateData (fromOracle (hitTest a (undefined :: b))) f a

-- | Find descendants of type @a@ non-transitively, while avoiding computation of areas that cannot contain values of
-- type @a@ using 'Data'.
--
-- 'uniplate' is a useful default definition for 'Control.Plated.plate'
uniplate :: Data a => Simple Traversal a a
uniplate = every
{-# INLINE uniplate #-}

-- | 'biplate' performs like 'every', except when @a ~ b@, it returns itself and nothing else.
--
biplate :: (Data a, Typeable b) => Simple Traversal a b
biplate f a = biplateData (fromOracle (hitTest a (undefined :: b))) f a

-------------------------------------------------------------------------------
-- Data Box
-------------------------------------------------------------------------------

data DataBox = forall a. Data a => DataBox
  { dataBoxKey :: TypeRep
  , _dataBoxVal :: a
  }

dataBox :: Data a => a -> DataBox
dataBox a = DataBox (typeOf a) a
{-# INLINE dataBox #-}

-- partial, caught elsewhere
sybChildren :: Data a => a -> [DataBox]
sybChildren x
  | isAlgType dt = do
    c <- dataTypeConstrs dt
    gmapQ dataBox (fromConstr c `asTypeOf` x)
  | otherwise = []
  where dt = dataTypeOf x

-------------------------------------------------------------------------------
-- HitMap
-------------------------------------------------------------------------------

type HitMap = HashMap TypeRep (HashSet TypeRep)

emptyHitMap :: HitMap
emptyHitMap = M.fromList
  [ (tRational, S.singleton tInteger)
  , (tInteger,  S.empty)
  ] where
  tRational = typeOf (undefined :: Rational)
  tInteger  = typeOf (undefined :: Integer )

insertHitMap :: DataBox -> HitMap -> HitMap
insertHitMap box hit = fixEq trans (populate box) <> hit where
  populate :: DataBox -> HitMap
  populate a = f a M.empty where
    f (DataBox k v) m
      | M.member k hit || M.member k m = m
      | cs <- sybChildren v = fs cs $ M.insert k (S.fromList $ map dataBoxKey cs) m
    fs []     m = m
    fs (x:xs) m = fs xs (f x m)

  trans :: HitMap -> HitMap
  trans m = M.map f m where
    f x = x <> foldMap g x
    g x = M.lookupDefault (hit ! x) x m

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = go where
  go x | x == x'   = x'
       | otherwise = go x'
       where x' = f x
{-# INLINE fixEq #-}

-- | inlineable 'unsafePerformIO'
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

-------------------------------------------------------------------------------
-- Cache
-------------------------------------------------------------------------------

data Cache = Cache HitMap (HashMap TypeRep (HashMap TypeRep (Maybe Follower)))

cache :: IORef Cache
cache = unsafePerformIO $ newIORef $ Cache emptyHitMap M.empty
{-# NOINLINE cache #-}

readCacheFollower :: DataBox -> TypeRep -> Maybe Follower
readCacheFollower b@(DataBox kb _) ka = inlinePerformIO $
  readIORef cache >>= \ (Cache hm m) -> case M.lookup kb m >>= M.lookup ka of
    Just a -> return a
    Nothing -> E.try (return $! insertHitMap b hm) >>= \r -> case r of
      Left SomeException{}                         -> atomicModifyIORef cache $ \(Cache hm' n) -> (Cache hm' (insert2 kb ka Nothing n), Nothing)
      Right hm' | fol <- Just (follower kb ka hm') -> atomicModifyIORef cache $ \(Cache _ n) -> (Cache hm' (insert2 kb ka fol n),    fol)

insert2 :: TypeRep -> TypeRep -> a -> HashMap TypeRep (HashMap TypeRep a) -> HashMap TypeRep (HashMap TypeRep a)
insert2 x y v = M.insertWith (const $ M.insert y v) x (M.singleton y v)
{-# INLINE insert2 #-}

{-
readCacheHitMap :: DataBox -> Maybe HitMap
readCacheHitMap b@(DataBox kb _) = inlinePerformIO $
  readIORef cache >>= \ (Cache hm _) -> case M.lookup kb hm of
    Just _  -> return $ Just hm
    Nothing -> E.try (return $! insertHitMap b hm) >>= \r -> case r of
      Left SomeException{} -> return Nothing
      Right hm' -> atomicModifyIORef cache $ \(Cache _ follow) -> (Cache hm' follow, Just hm')
-}

-------------------------------------------------------------------------------
-- Answers
-------------------------------------------------------------------------------

data Answer a
  = Hit a
  | Follow
  | Miss
  deriving (Eq,Ord,Show,Read)

instance Functor Answer where
  fmap f (Hit a) = Hit (f a)
  fmap _ Follow  = Follow
  fmap _ Miss    = Miss

-------------------------------------------------------------------------------
-- Oracles
-------------------------------------------------------------------------------

newtype Oracle a = Oracle { fromOracle :: forall t. Typeable t => t -> Answer a }

instance Functor Oracle where
  fmap f (Oracle g) = Oracle (fmap f . g)

hitTest :: (Data a, Typeable b) => a -> b -> Oracle b
hitTest a b
  | kb <- typeOf b = case readCacheFollower (dataBox a) kb of
    Nothing -> Oracle $ \c ->
      if typeOf c == kb
      then Hit (unsafeCoerce c)
      else Follow
    Just p -> Oracle $ \c -> let kc = typeOf c in
      if kc == kb then Hit (unsafeCoerce c)
      else if p kc then Follow
      else Miss

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------


biplateData :: forall f a b. (Applicative f, Data a, Typeable b) => (forall c. Typeable c => c -> Answer b) -> (b -> f b) -> a -> f a
biplateData o f a0 = go2 a0 where
  go :: Data d => d -> f d
  go a = gfoldl (\x y -> x <*> go2 y) pure a
  go2 :: Data d => d -> f d
  go2 a = case o a of
    Hit b  -> Unsafe.unsafeCoerce <$> f b
    Follow -> go a
    Miss   -> pure a
{-# INLINE biplateData #-}

uniplateData :: forall f a b. (Applicative f, Data a, Typeable b) => (forall c. Typeable c => c -> Answer b) -> (b -> f b) -> a -> f a
uniplateData o f a0 = go a0 where
  go :: Data d => d -> f d
  go a = gfoldl (\x y -> x <*> go2 y) pure a
  go2 :: Data d => d -> f d
  go2 a = case o a of
    Hit b  -> Unsafe.unsafeCoerce <$> f b
    Follow -> go a
    Miss   -> pure a
{-# INLINE uniplateData #-}

-------------------------------------------------------------------------------
-- Follower
-------------------------------------------------------------------------------

part :: (a -> Bool) -> HashSet a -> (HashSet a, HashSet a)
part p = S.filter p &&& S.filter (not . p)
{-# INLINE part #-}

type Follower = TypeRep -> Bool

follower :: TypeRep -> TypeRep -> HitMap -> Follower
follower a b m
  | S.null hit               = const False
  | S.null miss              = const True
  | S.size hit < S.size miss = \k -> S.member k hit
  | otherwise = \k -> not (S.member k miss)
  where (hit, miss) = part (\x -> S.member b (m ! x)) (S.insert a (m ! a))

