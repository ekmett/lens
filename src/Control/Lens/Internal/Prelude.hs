{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
#include "lens-common.h"
-- | Module which does most common imports (and related CPP)
-- needed across the lens library.
--
-- This module is intended to stay in other-modules of lens,
-- perfectly we'd just use @base-compat-batteries@
-- and not reinvent the wheel.
-- That's a reason why this module is different from
-- other .Internal modules, which are exposed-modules.
--
-- Also this is a "fat" Prelude, re-exporting commonly used,
-- non conflicting symbols.
--
module Control.Lens.Internal.Prelude
  ( module Prelude
  , Semigroup (..)
  , Monoid (..)
  , Foldable, foldMap, foldr, foldl, foldl', elem, null, length, traverse_
  , Traversable (..)
  , Applicative (..)
  , (&), (<&>), (<$>), (<$)
  -- * Data types
  , ZipList (..)
  , NonEmpty (..)
  -- * Functors
  , Identity (..)
  , Compose (..)
  , Const (..)
  -- * Control.Applicative
  , Alternative (..), WrappedMonad (..)
#if !MIN_VERSION_base(4,10,0)
  , liftA2
#endif
  -- * Data.Contravariant
  , Contravariant (..), phantom
  -- * Data.Monoid
  , Endo (..), Dual (..)
  -- * Data.Profunctor
  , Profunctor (..)
  , Choice (..), Cochoice (..)
  , Strong (..), Costrong (..)
  , Corepresentable (..)
  , Sieve (..), Cosieve (..)
  -- * Data.Proxy
  , Proxy (..)
  -- * Data.Tagged
  , Tagged (..)
  -- * Data.Void
  , Void, absurd
  -- * Data.Word
  , Word
  ) where

import Prelude hiding
    ( userError -- hiding something always helps with CPP
    , Applicative (..)
    , Foldable (..)
    , Traversable (..)
    , Monoid (..)
    , (<$>), (<$)
#if MIN_VERSION_base(4,13,0)
    , Semigroup (..)
#endif
    , Word
    )

-- Prelude
import Control.Applicative (Applicative (..), (<$>), (<$)) -- N.B. liftA2
import Data.Foldable (Foldable, foldMap, elem, foldr, foldl, foldl', traverse_) -- N.B. we don't define Foldable instances, so this way is makes less CPP
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Data.Word (Word)

-- Extras
import Data.Function ((&))
import Data.Foldable (length, null)

#if !MIN_VERSION_base(4,10,0)
import Control.Applicative (liftA2)
#endif

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif

import Control.Applicative (Alternative (..), Const (..), WrappedMonad (..), ZipList (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Contravariant (..), phantom)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Endo (..), Dual (..))
import Data.Profunctor (Strong (..), Choice (..), Cochoice (..), Costrong (..))
import Data.Profunctor.Rep (Corepresentable (..)) -- N.B. no Representable
import Data.Profunctor.Sieve (Sieve (..), Cosieve (..))
import Data.Profunctor.Unsafe (Profunctor (..))
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import Data.Void (Void, absurd)

-- TraversableWithIndex instances for tagged, vector and unordered-containers
-- We import this here, so the instances propagate through all (most) of @lens@.
import Data.Functor.WithIndex.Instances ()

#if !(MIN_VERSION_base(4,11,0))
-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
#endif
