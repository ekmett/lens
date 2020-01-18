{-# LANGUAGE CPP #-}
-- | Module which does most common imports (and related CPP)
-- needed across the lens library.
--
-- This module is intended to stay in other-modules of lens,
-- perfectly we'd just use @base-compat-batteries@
-- and not reinvent the wheel.
-- That's a reason why this module is different from
-- other .Internal modules, which are exposed-modules.
--
--
module Control.Lens.Internal.Prelude
  ( module Prelude
  , Semigroup (..)
  , Monoid (..)
  , Foldable, foldMap, foldr, foldl', null, length, traverse_
  , Traversable (..)
  , Applicative (..)
  , (&), (<&>), (<$>)
  -- * Functors
  , Identity (..)
  , Compose (..)
  , Const (..)
  -- * Data.Void
  , Void, absurd
  ) where

import Prelude hiding
    ( userError -- hiding something always helps with CPP
#if MIN_VERSION_base(4,8,0)
    , Applicative (..)
    , Foldable (..)
    , Traversable (..)
    , Monoid (..)
    , (<$>)
#else
    , foldr, length, null
    , mapM, sequence
#endif
#if MIN_VERSION_base(4,13,0)
    , Semigroup (..)
#endif
    )

import Control.Applicative (Applicative (..), (<$>)) -- N.B. liftA2
import Data.Foldable (Foldable, foldMap, foldr, foldl', traverse_) -- N.B. we don't define Foldable instances, so this way is makes less CPP
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))

#if MIN_VERSION_base(4,8,0)
import Data.Function ((&))
import Data.Foldable (length, null)
#endif

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Compose (Compose (..))
import Data.Void (Void, absurd)

-- $setup
-- >>> import Control.Lens
-- >>> import Control.Monad.State
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h

#if !(MIN_VERSION_base(4,8,0))
-- | Passes the result of the left side to the function on the right side (forward pipe operator).
--
-- This is the flipped version of ('$'), which is more common in languages like F# as (@|>@) where it is needed
-- for inference. Here it is supplied for notational convenience and given a precedence that allows it
-- to be nested inside uses of ('$').
--
-- >>> a & f
-- f a
--
-- >>> "hello" & length & succ
-- 6
--
-- This combinator is commonly used when applying multiple 'Lens' operations in sequence.
--
-- >>> ("hello","world") & _1.element 0 .~ 'j' & _1.element 4 .~ 'y'
-- ("jelly","world")
--
-- This reads somewhat similar to:
--
-- >>> flip execState ("hello","world") $ do _1.element 0 .= 'j'; _1.element 4 .= 'y'
-- ("jelly","world")
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &

null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif

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
