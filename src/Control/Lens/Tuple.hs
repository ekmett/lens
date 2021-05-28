{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

#include "lens-common.h"

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Tuple
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-------------------------------------------------------------------------------
module Control.Lens.Tuple
  (
  -- * Tuples
    Field1(..)
  , Field2(..)
  , Field3(..)
  , Field4(..)
  , Field5(..)
  , Field6(..)
  , Field7(..)
  , Field8(..)
  , Field9(..)
  , Field10(..)
  , Field11(..)
  , Field12(..)
  , Field13(..)
  , Field14(..)
  , Field15(..)
  , Field16(..)
  , Field17(..)
  , Field18(..)
  , Field19(..)
  -- * Strict variations
  , _1', _2', _3', _4', _5', _6', _7', _8', _9'
  , _10', _11', _12', _13', _14', _15', _16'
  , _17', _18', _19'
  ) where

import           Prelude ()
import           Control.Lens.Lens
import           Control.Lens.Internal.Prelude
import           Data.Functor.Product  (Product (..))
import           Data.Kind
import           Data.Strict           (Pair (..))
import           GHC.Generics          ((:*:) (..), Generic (..), K1 (..),
                                        M1 (..), U1 (..))

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | Provides access to 1st field of a tuple.
class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 1st field of a tuple (and possibly change its type).
  --
  -- >>> (1,2)^._1
  -- 1
  --
  -- >>> _1 .~ "hello" $ (1,2)
  -- ("hello",2)
  --
  -- >>> (1,2) & _1 .~ "hello"
  -- ("hello",2)
  --
  -- >>> _1 putStrLn ("hello","world")
  -- hello
  -- ((),"world")
  --
  -- This can also be used on larger tuples as well:
  --
  -- >>> (1,2,3,4,5) & _1 +~ 41
  -- (42,2,3,4,5)
  --
  -- @
  -- '_1' :: 'Lens' (a,b) (a',b) a a'
  -- '_1' :: 'Lens' (a,b,c) (a',b,c) a a'
  -- '_1' :: 'Lens' (a,b,c,d) (a',b,c,d) a a'
  -- ...
  -- '_1' :: 'Lens' (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a'
  -- @
  _1 :: Lens s t a b
  default _1 :: (Generic s, Generic t, GIxed N0 (Rep s) (Rep t) a b)
             => Lens s t a b
  _1 = ix proxyN0
  {-# INLINE _1 #-}

instance Field1 (Identity a) (Identity b) a b where
  _1 f (Identity a) = Identity <$> f a

instance Field1 (Product f g a) (Product f' g a) (f a) (f' a) where
  _1 f (Pair a b) = flip Pair b <$> f a

instance Field1 ((f :*: g) p) ((f' :*: g) p) (f p) (f' p) where
  _1 f (l :*: r) = (:*: r) <$> f l

-- | @since 4.20
instance Field1 (Pair a b) (Pair a' b) a a' where
  _1 f (a :!: b) = (:!: b) <$> f a

-- | @
-- '_1' k ~(a,b) = (\\a' -> (a',b)) 'Data.Functor.<$>' k a
-- @
instance Field1 (a,b) (a',b) a a' where
  _1 k ~(a,b) = k a <&> \a' -> (a',b)
  {-# INLINE _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 k ~(a,b,c) = k a <&> \a' -> (a',b,c)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 k ~(a,b,c,d) = k a <&> \a' -> (a',b,c,d)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 k ~(a,b,c,d,e) = k a <&> \a' -> (a',b,c,d,e)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 k ~(a,b,c,d,e,f) = k a <&> \a' -> (a',b,c,d,e,f)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) a a' where
  _1 k ~(a,b,c,d,e,f,g) = k a <&> \a' -> (a',b,c,d,e,f,g)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) a a' where
  _1 k ~(a,b,c,d,e,f,g,h) = k a <&> \a' -> (a',b,c,d,e,f,g,h)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j) (a',b,c,d,e,f,g,h,i,j) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk) (a',b,c,d,e,f,g,h,i,j,kk) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l) (a',b,c,d,e,f,g,h,i,j,kk,l) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a',b,c,d,e,f,g,h,i,j,kk,l,m) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k a <&> \a' -> (a',b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _1 #-}

-- | Provides access to the 2nd field of a tuple.
class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 2nd field of a tuple.
  --
  -- >>> _2 .~ "hello" $ (1,(),3,4)
  -- (1,"hello",3,4)
  --
  -- >>> (1,2,3,4) & _2 *~ 3
  -- (1,6,3,4)
  --
  -- >>> _2 print (1,2)
  -- 2
  -- (1,())
  --
  -- @
  -- 'Control.Lens.Fold.anyOf' '_2' :: (s -> 'Bool') -> (a, s) -> 'Bool'
  -- 'Data.Traversable.traverse' '.' '_2' :: ('Control.Applicative.Applicative' f, 'Data.Traversable.Traversable' t) => (a -> f b) -> t (s, a) -> f (t (s, b))
  -- 'Control.Lens.Fold.foldMapOf' ('Data.Traversable.traverse' '.' '_2') :: ('Data.Traversable.Traversable' t, 'Data.Monoid.Monoid' m) => (s -> m) -> t (b, s) -> m
  -- @
  _2 :: Lens s t a b
  default _2 :: (Generic s, Generic t, GIxed N1 (Rep s) (Rep t) a b)
             => Lens s t a b
  _2 = ix proxyN1
  {-# INLINE _2 #-}

instance Field2 (Product f g a) (Product f g' a) (g a) (g' a) where
  _2 f (Pair a b) = Pair a <$> f b

instance Field2 ((f :*: g) p) ((f :*: g') p) (g p) (g' p) where
  _2 f (l :*: r) = (l :*:) <$> f r

-- | @since 4.20
instance Field2 (Pair a b) (Pair a b') b b' where
  _2 f (a :!: b) = (a :!:) <$> f b

-- | @
-- '_2' k ~(a,b) = (\\b' -> (a,b')) 'Data.Functor.<$>' k b
-- @
instance Field2 (a,b) (a,b') b b' where
  _2 k ~(a,b) = k b <&> \b' -> (a,b')
  {-# INLINE _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 k ~(a,b,c) = k b <&> \b' -> (a,b',c)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 k ~(a,b,c,d) = k b <&> \b' -> (a,b',c,d)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 k ~(a,b,c,d,e) = k b <&> \b' -> (a,b',c,d,e)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 k ~(a,b,c,d,e,f) = k b <&> \b' -> (a,b',c,d,e,f)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) b b' where
  _2 k ~(a,b,c,d,e,f,g) = k b <&> \b' -> (a,b',c,d,e,f,g)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) b b' where
  _2 k ~(a,b,c,d,e,f,g,h) = k b <&> \b' -> (a,b',c,d,e,f,g,h)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j) (a,b',c,d,e,f,g,h,i,j) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk) (a,b',c,d,e,f,g,h,i,j,kk) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b',c,d,e,f,g,h,i,j,kk,l) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b',c,d,e,f,g,h,i,j,kk,l,m) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k b <&> \b' -> (a,b',c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _2 #-}


-- | Provides access to the 3rd field of a tuple.
class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 3rd field of a tuple.
  _3 :: Lens s t a b
  default _3 :: (Generic s, Generic t, GIxed N2 (Rep s) (Rep t) a b)
             => Lens s t a b
  _3 = ix proxyN2
  {-# INLINE _3 #-}

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 k ~(a,b,c) = k c <&> \c' -> (a,b,c')
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 k ~(a,b,c,d) = k c <&> \c' -> (a,b,c',d)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 k ~(a,b,c,d,e) = k c <&> \c' -> (a,b,c',d,e)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f) (a,b,c',d,e,f) c c' where
  _3 k ~(a,b,c,d,e,f) = k c <&> \c' -> (a,b,c',d,e,f)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) c c' where
  _3 k ~(a,b,c,d,e,f,g) = k c <&> \c' -> (a,b,c',d,e,f,g)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) c c' where
  _3 k ~(a,b,c,d,e,f,g,h) = k c <&> \c' -> (a,b,c',d,e,f,g,h)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j) (a,b,c',d,e,f,g,h,i,j) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c',d,e,f,g,h,i,j,kk) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c',d,e,f,g,h,i,j,kk,l) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c',d,e,f,g,h,i,j,kk,l,m) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k c <&> \c' -> (a,b,c',d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _3 #-}

-- | Provide access to the 4th field of a tuple.
class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 4th field of a tuple.
  _4 :: Lens s t a b
  default _4 :: (Generic s, Generic t, GIxed N3 (Rep s) (Rep t) a b)
             => Lens s t a b
  _4 = ix proxyN3
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 k ~(a,b,c,d) = k d <&> \d' -> (a,b,c,d')
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 k ~(a,b,c,d,e) = k d <&> \d' -> (a,b,c,d',e)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f) (a,b,c,d',e,f) d d' where
  _4 k ~(a,b,c,d,e,f) = k d <&> \d' -> (a,b,c,d',e,f)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) d d' where
  _4 k ~(a,b,c,d,e,f,g) = k d <&> \d' -> (a,b,c,d',e,f,g)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) d d' where
  _4 k ~(a,b,c,d,e,f,g,h) = k d <&> \d' -> (a,b,c,d',e,f,g,h)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d',e,f,g,h,i,j) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d',e,f,g,h,i,j,kk) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d',e,f,g,h,i,j,kk,l) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d',e,f,g,h,i,j,kk,l,m) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q,r) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k d <&> \d' -> (a,b,c,d',e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _4 #-}

-- | Provides access to the 5th field of a tuple.
class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 5th field of a tuple.
  _5 :: Lens s t a b
  default _5 :: (Generic s, Generic t, GIxed N4 (Rep s) (Rep t) a b)
             => Lens s t a b
  _5 = ix proxyN4
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 k ~(a,b,c,d,e) = k e <&> \e' -> (a,b,c,d,e')
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f) (a,b,c,d,e',f) e e' where
  _5 k ~(a,b,c,d,e,f) = k e <&> \e' -> (a,b,c,d,e',f)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) e e' where
  _5 k ~(a,b,c,d,e,f,g) = k e <&> \e' -> (a,b,c,d,e',f,g)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) e e' where
  _5 k ~(a,b,c,d,e,f,g,h) = k e <&> \e' -> (a,b,c,d,e',f,g,h)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e',f,g,h,i,j) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e',f,g,h,i,j,kk) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e',f,g,h,i,j,kk,l) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e',f,g,h,i,j,kk,l,m) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q,r) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q,r,s) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k e <&> \e' -> (a,b,c,d,e',f,g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _5 #-}

-- | Provides access to the 6th element of a tuple.
class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 6th field of a tuple.
  _6 :: Lens s t a b
  default _6 :: (Generic s, Generic t, GIxed N5 (Rep s) (Rep t) a b)
             => Lens s t a b
  _6 = ix proxyN5
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f) (a,b,c,d,e,f') f f' where
  _6 k ~(a,b,c,d,e,f) = k f <&> \f' -> (a,b,c,d,e,f')
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g) (a,b,c,d,e,f',g) f f' where
  _6 k ~(a,b,c,d,e,f,g) = k f <&> \f' -> (a,b,c,d,e,f',g)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f',g,h) f f' where
  _6 k ~(a,b,c,d,e,f,g,h) = k f <&> \f' -> (a,b,c,d,e,f',g,h)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f',g,h,i) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f',g,h,i,j) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f',g,h,i,j,kk) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f',g,h,i,j,kk,l) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f',g,h,i,j,kk,l,m) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q,r) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q,r,s) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k f <&> \f' -> (a,b,c,d,e,f',g,h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _6 #-}

-- | Provide access to the 7th field of a tuple.
class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 7th field of a tuple.
  _7 :: Lens s t a b
  default _7 :: (Generic s, Generic t, GIxed N6 (Rep s) (Rep t) a b)
             => Lens s t a b
  _7 = ix proxyN6
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g) (a,b,c,d,e,f,g') g g' where
  _7 k ~(a,b,c,d,e,f,g) = k g <&> \g' -> (a,b,c,d,e,f,g')
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g',h) g g' where
  _7 k ~(a,b,c,d,e,f,g,h) = k g <&> \g' -> (a,b,c,d,e,f,g',h)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g',h,i) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f,g',h,i,j) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f,g',h,i,j,kk) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g',h,i,j,kk,l) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g',h,i,j,kk,l,m) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q,r) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q,r,s) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k g <&> \g' -> (a,b,c,d,e,f,g',h,i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _7 #-}

-- | Provide access to the 8th field of a tuple.
class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 8th field of a tuple.
  _8 :: Lens s t a b
  default _8 :: (Generic s, Generic t, GIxed N7 (Rep s) (Rep t) a b)
             => Lens s t a b
  _8 = ix proxyN7
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h') h h' where
  _8 k ~(a,b,c,d,e,f,g,h) = k h <&> \h' -> (a,b,c,d,e,f,g,h')
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h',i) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f,g,h',i,j) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f,g,h',i,j,kk) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g,h',i,j,kk,l) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h',i,j,kk,l,m) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q,r) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q,r)
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q,r,s) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k h <&> \h' -> (a,b,c,d,e,f,g,h',i,j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _8 #-}

-- | Provides access to the 9th field of a tuple.
class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 9th field of a tuple.
  _9 :: Lens s t a b
  default _9 :: (Generic s, Generic t, GIxed N8 (Rep s) (Rep t) a b)
             => Lens s t a b
  _9 = ix proxyN8
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i') i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i')
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f,g,h,i',j) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f,g,h,i',j,kk) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g,h,i',j,kk,l) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h,i',j,kk,l,m) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q,r) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q,r)
  {-# INLINE _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q,r,s) i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k i <&> \i' -> (a,b,c,d,e,f,g,h,i',j,kk,l,m,n,o,p,q,r,s)
  {-# INLINE _9 #-}

-- | Provides access to the 10th field of a tuple.
class Field10 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 10th field of a tuple.
  _10 :: Lens s t a b
  default _10 :: (Generic s, Generic t, GIxed N9 (Rep s) (Rep t) a b)
             => Lens s t a b
  _10 = ix proxyN9
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f,g,h,i,j') j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j')
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f,g,h,i,j',kk) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g,h,i,j',kk,l) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h,i,j',kk,l,m) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q,r) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q,r)
  {-# INLINE _10 #-}

instance Field10 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q,r,s) j j' where
  _10 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k j <&> \j' -> (a,b,c,d,e,f,g,h,i,j',kk,l,m,n,o,p,q,r,s)
  {-# INLINE _10 #-}

-- | Provides access to the 11th field of a tuple.
class Field11 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 11th field of a tuple.
  _11 :: Lens s t a b
  default _11 :: (Generic s, Generic t, GIxed N10 (Rep s) (Rep t) a b)
             => Lens s t a b
  _11 = ix proxyN10
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk) (a,b,c,d,e,f,g,h,i,j,kk') kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk')
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g,h,i,j,kk',l) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h,i,j,kk',l,m) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q,r) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q,r)
  {-# INLINE _11 #-}

instance Field11 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q,r,s) kk kk' where
  _11 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k kk <&> \kk' -> (a,b,c,d,e,f,g,h,i,j,kk',l,m,n,o,p,q,r,s)
  {-# INLINE _11 #-}

-- | Provides access to the 12th field of a tuple.
class Field12 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 12th field of a tuple.
  _12 :: Lens s t a b
  default _12 :: (Generic s, Generic t, GIxed N11 (Rep s) (Rep t) a b)
             => Lens s t a b
  _12 = ix proxyN11
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l) (a,b,c,d,e,f,g,h,i,j,kk,l') l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l')
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h,i,j,kk,l',m) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q,r) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q,r)
  {-# INLINE _12 #-}

instance Field12 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q,r,s) l l' where
  _12 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k l <&> \l' -> (a,b,c,d,e,f,g,h,i,j,kk,l',m,n,o,p,q,r,s)
  {-# INLINE _12 #-}

-- | Provides access to the 13th field of a tuple.
class Field13 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 13th field of a tuple.
  _13 :: Lens s t a b
  default _13 :: (Generic s, Generic t, GIxed N12 (Rep s) (Rep t) a b)
             => Lens s t a b
  _13 = ix proxyN12
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m) (a,b,c,d,e,f,g,h,i,j,kk,l,m') m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m')
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n)
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o)
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p)
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q)
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q,r) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q,r)
  {-# INLINE _13 #-}

instance Field13 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q,r,s) m m' where
  _13 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k m <&> \m' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m',n,o,p,q,r,s)
  {-# INLINE _13 #-}

-- | Provides access to the 14th field of a tuple.
class Field14 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 14th field of a tuple.
  _14 :: Lens s t a b
  default _14 :: (Generic s, Generic t, GIxed N13 (Rep s) (Rep t) a b)
             => Lens s t a b
  _14 = ix proxyN13
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n') n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n')
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o) n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o)
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p) n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p)
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q) n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q)
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q,r) n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q,r)
  {-# INLINE _14 #-}

instance Field14 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q,r,s) n n' where
  _14 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k n <&> \n' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n',o,p,q,r,s)
  {-# INLINE _14 #-}

-- | Provides access to the 15th field of a tuple.
class Field15 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 15th field of a tuple.
  _15 :: Lens s t a b
  default _15 :: (Generic s, Generic t, GIxed N14 (Rep s) (Rep t) a b)
             => Lens s t a b
  _15 = ix proxyN14
  {-# INLINE _15 #-}

instance Field15 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o') o o' where
  _15 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o) = k o <&> \o' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o')
  {-# INLINE _15 #-}

instance Field15 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p) o o' where
  _15 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k o <&> \o' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p)
  {-# INLINE _15 #-}

instance Field15 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q) o o' where
  _15 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k o <&> \o' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q)
  {-# INLINE _15 #-}

instance Field15 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q,r) o o' where
  _15 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k o <&> \o' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q,r)
  {-# INLINE _15 #-}

instance Field15 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q,r,s) o o' where
  _15 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k o <&> \o' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o',p,q,r,s)
  {-# INLINE _15 #-}

-- | Provides access to the 16th field of a tuple.
class Field16 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 16th field of a tuple.
  _16 :: Lens s t a b
  default _16 :: (Generic s, Generic t, GIxed N15 (Rep s) (Rep t) a b)
             => Lens s t a b
  _16 = ix proxyN15
  {-# INLINE _16 #-}

instance Field16 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p') p p' where
  _16 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p) = k p <&> \p' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p')
  {-# INLINE _16 #-}

instance Field16 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q) p p' where
  _16 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k p <&> \p' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q)
  {-# INLINE _16 #-}

instance Field16 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q,r) p p' where
  _16 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k p <&> \p' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q,r)
  {-# INLINE _16 #-}

instance Field16 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q,r,s) p p' where
  _16 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k p <&> \p' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p',q,r,s)
  {-# INLINE _16 #-}

-- | Provides access to the 17th field of a tuple.
class Field17 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 17th field of a tuple.
  _17 :: Lens s t a b
  default _17 :: (Generic s, Generic t, GIxed N16 (Rep s) (Rep t) a b)
             => Lens s t a b
  _17 = ix proxyN16
  {-# INLINE _17 #-}

instance Field17 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q') q q' where
  _17 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q) = k q <&> \q' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q')
  {-# INLINE _17 #-}

instance Field17 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q',r) q q' where
  _17 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k q <&> \q' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q',r)
  {-# INLINE _17 #-}

instance Field17 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q',r,s) q q' where
  _17 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k q <&> \q' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q',r,s)
  {-# INLINE _17 #-}

-- | Provides access to the 18th field of a tuple.
class Field18 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 18th field of a tuple.
  _18 :: Lens s t a b
  default _18 :: (Generic s, Generic t, GIxed N17 (Rep s) (Rep t) a b)
             => Lens s t a b
  _18 = ix proxyN17
  {-# INLINE _18 #-}

instance Field18 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r') r r' where
  _18 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r) = k r <&> \r' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r')
  {-# INLINE _18 #-}

instance Field18 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r',s) r r' where
  _18 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k r <&> \r' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r',s)
  {-# INLINE _18 #-}

-- | Provides access to the 19th field of a tuple.
class Field19 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 19th field of a tuple.
  _19 :: Lens s t a b
  default _19 :: (Generic s, Generic t, GIxed N18 (Rep s) (Rep t) a b)
             => Lens s t a b
  _19 = ix proxyN18
  {-# INLINE _19 #-}

instance Field19 (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s') s s' where
  _19 k ~(a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s) = k s <&> \s' -> (a,b,c,d,e,f,g,h,i,j,kk,l,m,n,o,p,q,r,s')
  {-# INLINE _19 #-}

-- Strict versions of the _1 .. _19 operations

-- | Strict version of '_1'
_1' :: Field1 s t a b => Lens s t a b
_1' = \f !x -> _1 f x
{-# INLINE _1' #-}

-- | Strict version of '_2'
_2' :: Field2 s t a b => Lens s t a b
_2' = \f !x -> _2 f x
{-# INLINE _2' #-}

-- | Strict version of '_3'
_3' :: Field3 s t a b => Lens s t a b
_3' = \f !x -> _3 f x
{-# INLINE _3' #-}

-- | Strict version of '_4'
_4' :: Field4 s t a b => Lens s t a b
_4' = \f !x -> _4 f x
{-# INLINE _4' #-}

-- | Strict version of '_5'
_5' :: Field5 s t a b => Lens s t a b
_5' = \f !x -> _5 f x
{-# INLINE _5' #-}

-- | Strict version of '_6'
_6' :: Field6 s t a b => Lens s t a b
_6' = \f !x -> _6 f x
{-# INLINE _6' #-}

-- | Strict version of '_7'
_7' :: Field7 s t a b => Lens s t a b
_7' = \f !x -> _7 f x
{-# INLINE _7' #-}

-- | Strict version of '_8'
_8' :: Field8 s t a b => Lens s t a b
_8' = \f !x -> _8 f x
{-# INLINE _8' #-}

-- | Strict version of '_9'
_9' :: Field9 s t a b => Lens s t a b
_9' = \f !x -> _9 f x
{-# INLINE _9' #-}

-- | Strict version of '_10'
_10' :: Field10 s t a b => Lens s t a b
_10' = \f !x -> _10 f x
{-# INLINE _10' #-}

-- | Strict version of '_11'
_11' :: Field11 s t a b => Lens s t a b
_11' = \f !x -> _11 f x
{-# INLINE _11' #-}

-- | Strict version of '_12'
_12' :: Field12 s t a b => Lens s t a b
_12' = \f !x -> _12 f x
{-# INLINE _12' #-}

-- | Strict version of '_13'
_13' :: Field13 s t a b => Lens s t a b
_13' = \f !x -> _13 f x
{-# INLINE _13' #-}

-- | Strict version of '_14'
_14' :: Field14 s t a b => Lens s t a b
_14' = \f !x -> _14 f x
{-# INLINE _14' #-}

-- | Strict version of '_15'
_15' :: Field15 s t a b => Lens s t a b
_15' = \f !x -> _15 f x
{-# INLINE _15' #-}

-- | Strict version of '_16'
_16' :: Field16 s t a b => Lens s t a b
_16' = \f !x -> _16 f x
{-# INLINE _16' #-}

-- | Strict version of '_17'
_17' :: Field17 s t a b => Lens s t a b
_17' = \f !x -> _17 f x
{-# INLINE _17' #-}

-- | Strict version of '_18'
_18' :: Field18 s t a b => Lens s t a b
_18' = \f !x -> _18 f x
{-# INLINE _18' #-}

-- | Strict version of '_19'
_19' :: Field19 s t a b => Lens s t a b
_19' = \f !x -> _19 f x
{-# INLINE _19' #-}


ix :: (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b) => f n -> Lens s t a b
ix n f = fmap to . gix n f . from
{-# INLINE ix #-}

type family GSize (f :: Type -> Type)
type instance GSize U1 = Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :*: b) = Add (GSize a) (GSize b)

class GIxed n s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gix :: f n -> Lens (s x) (t x) a b

instance GIxed N0 (K1 i a) (K1 i b) a b where
  gix _ = dimap unK1 (fmap K1)
  {-# INLINE gix #-}

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  gix n = dimap unM1 (fmap M1) . gix n
  {-# INLINE gix #-}

instance (p ~ GT (GSize s) n,
          p ~ GT (GSize t) n,
          GIxed' p n s s' t t' a b)
      => GIxed n (s :*: s') (t :*: t') a b where
  gix = gix' (Proxy :: Proxy p)
  {-# INLINE gix #-}

-- $gixed-fundeps
-- >>> :set -XDeriveGeneric -XFlexibleInstances -XMultiParamTypeClasses
-- >>> import GHC.Generics (Generic)
-- >>> data Product a b = a :* b deriving Generic
-- >>> instance Field1 (Product a b) (Product a' b) a a'
-- >>> instance Field2 (Product a b) (Product a b') b b'

class (p ~ GT (GSize s) n,
       p ~ GT (GSize t) n)
   => GIxed' p n s s' t t' a b | p n s s' -> a
                               , p n t t' -> b
                               , p n s s' b -> t t'
                               , p n t t' a -> s s' where
  gix' :: f p -> g n -> Lens ((s :*: s') x) ((t :*: t') x) a b

instance (GT (GSize s) n ~ T,
          GT (GSize t) n ~ T,
          GIxed n s t a b)
      => GIxed' T n s s' t s' a b where
  gix' _ n f (s :*: s') = (:*: s') <$> gix n f s
  {-# INLINE gix' #-}

instance (GT (GSize s) n ~ F,
          n' ~ Subtract (GSize s) n,
          GIxed n' s' t' a b)
      => GIxed' F n s s' s t' a b where
  gix' _ _  f (s :*: s') = (s :*:) <$> gix (Proxy :: Proxy n') f s'
  {-# INLINE gix' #-}

data Z
data S a

data T
data F

type family Add x y
type instance Add Z y = y
type instance Add (S x) y = S (Add x y)

type family Subtract x y
type instance Subtract Z x = x
type instance Subtract (S x) (S y) = Subtract x y

type family GT x y
type instance GT Z x = F
type instance GT (S x) Z = T
type instance GT (S x) (S y) = GT x y

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8
type N10 = S N9
type N11 = S N10
type N12 = S N11
type N13 = S N12
type N14 = S N13
type N15 = S N14
type N16 = S N15
type N17 = S N16
type N18 = S N17

proxyN0 :: Proxy N0
proxyN0 = Proxy
{-# INLINE proxyN0 #-}

proxyN1 :: Proxy N1
proxyN1 = Proxy
{-# INLINE proxyN1 #-}

proxyN2 :: Proxy N2
proxyN2 = Proxy
{-# INLINE proxyN2 #-}

proxyN3 :: Proxy N3
proxyN3 = Proxy
{-# INLINE proxyN3 #-}

proxyN4 :: Proxy N4
proxyN4 = Proxy
{-# INLINE proxyN4 #-}

proxyN5 :: Proxy N5
proxyN5 = Proxy
{-# INLINE proxyN5 #-}

proxyN6 :: Proxy N6
proxyN6 = Proxy
{-# INLINE proxyN6 #-}

proxyN7 :: Proxy N7
proxyN7 = Proxy
{-# INLINE proxyN7 #-}

proxyN8 :: Proxy N8
proxyN8 = Proxy
{-# INLINE proxyN8 #-}

proxyN9 :: Proxy N9
proxyN9 = Proxy
{-# INLINE proxyN9 #-}

proxyN10 :: Proxy N10
proxyN10 = Proxy
{-# INLINE proxyN10 #-}

proxyN11 :: Proxy N11
proxyN11 = Proxy
{-# INLINE proxyN11 #-}

proxyN12 :: Proxy N12
proxyN12 = Proxy
{-# INLINE proxyN12 #-}

proxyN13 :: Proxy N13
proxyN13 = Proxy
{-# INLINE proxyN13 #-}

proxyN14 :: Proxy N14
proxyN14 = Proxy
{-# INLINE proxyN14 #-}

proxyN15 :: Proxy N15
proxyN15 = Proxy
{-# INLINE proxyN15 #-}

proxyN16 :: Proxy N16
proxyN16 = Proxy
{-# INLINE proxyN16 #-}

proxyN17 :: Proxy N17
proxyN17 = Proxy
{-# INLINE proxyN17 #-}

proxyN18 :: Proxy N18
proxyN18 = Proxy
{-# INLINE proxyN18 #-}
