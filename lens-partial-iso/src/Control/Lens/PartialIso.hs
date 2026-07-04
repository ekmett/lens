{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.PartialIso
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  RankNTypes
--
-- A 'Prism' matches one way and builds the other: matching may fail, but
-- building (a 'review') always succeeds. This module provides two optics with
-- the opposite flavor of partiality, useful when describing reversible
-- parsing\/building (codecs):
--
-- * An 'InvPrism' is a 'Prism' \"turned around\": the get direction always
--   succeeds, but the build direction (the 'review') may fail.
--
-- * A 'PartialIso' is an 'Iso' where both directions may fail.
--
-- Both rely on a 'Filterable' result functor (from the @witherable@ package),
-- which is a 'Functor' you can also drop elements out of
-- (@'mapMaybe' :: (a -> 'Maybe' b) -> f a -> f b@). That extra power is exactly
-- what lets the build direction fail: where a 'Prism' uses 'Applicative'\'s
-- 'pure' to always produce a result, these optics use 'mapMaybe'\/'catMaybes'
-- to produce no result.
-------------------------------------------------------------------------------
module Control.Lens.PartialIso
  (
  -- * Inverse Prisms
    InvPrism, InvPrism'
  , invPrism
  -- * Partial Isos
  , PartialIso, PartialIso'
  , partialIso
  -- * Combinators
  , failing'
  , (#?)
  -- * Filterable
  --
  -- | We re-export the 'Filterable' methods used to build these optics, but not
  -- the rest of the class — in particular @filter@, which would clash with
  -- "Prelude".'Prelude.filter'. Import "Witherable" directly for the others.
  , Filterable(mapMaybe, catMaybes)
  ) where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged (..))
import Witherable (Filterable (mapMaybe, catMaybes))

-- $setup
-- >>> :set -XRankNTypes
-- >>> import Control.Lens
-- >>> import Control.Lens.PartialIso
-- >>> import Text.Read (readMaybe)
-- >>> let intPI = partialIso (readMaybe :: String -> Maybe Int) (Just . show) :: PartialIso' String Int

infixr 8 #?

-- | A 'Prism' \"turned around\": a total getter one way, but a partial 'review'
-- the other way.
--
-- Compare the encoding to 'Prism':
--
-- @
-- type 'Prism'    s t a b = forall p f. ('Choice' p,     'Applicative' f) => 'Optic' p f s t a b
-- type 'InvPrism' s t a b = forall p f. ('Profunctor' p, 'Filterable' f)  => 'Optic' p f s t a b
-- @
--
-- A 'Prism' wants 'Choice' (so matching may fail) and 'Applicative' (so
-- building always succeeds). An 'InvPrism' wants only 'Profunctor' (so getting
-- is total) and 'Filterable' (so building may now fail).
--
-- /Laws:/ an 'InvPrism' @i@ is well-behaved exactly when it is 'invPrism' of a
-- lawful 'Prism'; the inverse-prism laws are then the 'Prism' laws with the get
-- and build directions exchanged. The total get is @'view' ('getting' i)@ and
-- the partial build is @i '#?' _@; for a simple @'InvPrism'' s a@ they satisfy:
--
-- @
-- i #? 'view' ('getting' i) s ≡ 'Just' s
-- i #? a ≡ 'Just' s   ⟹   'view' ('getting' i) s ≡ a
-- @
--
-- Note that an 'InvPrism' does not carry all the power of a 'Prism': it cannot
-- act as an \"inverted setter\", because its build may simply fail rather than
-- fall back to a type-changing update.
type InvPrism s t a b =
  forall p f. (Profunctor p, Filterable f) => Optic p f s t a b

-- | A 'Simple' 'InvPrism'.
type InvPrism' s a = InvPrism s s a a

-- | A partial isomorphism: a partial getter and a partial 'review', i.e.
-- conversion in either direction may fail.
--
-- @
-- type 'Iso'        s t a b = forall p f. ('Profunctor' p, 'Functor' f)                 => 'Optic' p f s t a b
-- type 'PartialIso' s t a b = forall p f. ('Choice' p, 'Applicative' f, 'Filterable' f) => 'Optic' p f s t a b
-- @
--
-- Relative to an 'Iso' this adds 'Choice' (matching may fail) and 'Filterable'
-- (building may fail). Composing a 'Prism' with an 'InvPrism' yields a
-- 'PartialIso'.
--
-- /Laws:/ a simple @'PartialIso'' s a@ presents two partial functions, a
-- forward @sma :: s -> 'Maybe' a@ (the get) and a backward @ams :: a -> 'Maybe'
-- s@ (the build). They must be mutually inverse where defined:
--
-- @
-- sma s ≡ 'Just' a   ⟹   ams a ≡ 'Just' s
-- ams a ≡ 'Just' s   ⟹   sma s ≡ 'Just' a
-- @
--
-- (For the type-changing @'PartialIso' s t a b@ the laws generalize exactly as
-- the 'Iso' laws do.) When @sma@ and @ams@ are total this collapses to the two
-- 'Iso' round-trip laws.
type PartialIso s t a b =
  forall p f. (Choice p, Applicative f, Filterable f) => Optic p f s t a b

-- | A 'Simple' 'PartialIso'.
type PartialIso' s a = PartialIso s s a a

-- | A partial variant of @('#')@ for partial reviews: build a @t@ from a @b@,
-- which may fail.
--
-- @
-- ('#?') :: 'PartialIso' s t a b -> b -> 'Maybe' t
-- ('#?') :: 'InvPrism'   s t a b -> b -> 'Maybe' t
-- @
--
-- >>> intPI #? 5
-- Just "5"
(#?) :: Optic Tagged Maybe s t a b -> b -> Maybe t
f #? b = Just b & Tagged & f & unTagged
{-# INLINE (#?) #-}

-- | Turn an 'APrism' around into an 'InvPrism': the prism's 'review' becomes a
-- total getter, and the prism's match becomes a partial 'review'.
--
-- @
-- 'invPrism' :: 'APrism' b a t s -> 'InvPrism' s t a b
-- @
--
-- /Laws:/ if @p@ is a lawful 'Prism' then @'invPrism' p@ is a lawful
-- 'InvPrism' — its laws are precisely @p@'s 'Prism' laws with the get and
-- build directions exchanged.
--
-- >>> (3 :: Int) ^? getting (invPrism _Left) :: Maybe (Either Int String)
-- Just (Left 3)
invPrism :: APrism b a t s -> InvPrism s t a b
invPrism p =
  dimap
  (review (reviewing (clonePrism p)))
  (mapMaybe (^? getting (clonePrism p)))
{-# INLINE invPrism #-}

-- | Build a 'PartialIso' from two partial conversions: a forward @sma@ (the
-- get) and a backward @ams@ (the build).
--
-- /Laws:/ the result is a lawful 'PartialIso' when @sma@ and @ams@ are mutually
-- inverse partial functions (see 'PartialIso'). For a simple
-- @'PartialIso'' s a@:
--
-- @
-- sma s ≡ 'Just' a   ⟹   ams a ≡ 'Just' s
-- ams a ≡ 'Just' s   ⟹   sma s ≡ 'Just' a
-- @
--
-- >>> "42" ^? getting intPI
-- Just 42
--
-- >>> "frob" ^? getting intPI
-- Nothing
--
-- >>> intPI #? 7
-- Just "7"
partialIso :: (s -> Maybe a) -> (b -> Maybe t) -> PartialIso s t a b
partialIso sma ams =
  dimap
  (maybe (Left ()) Right . sma)
  (catMaybes . either (const (pure Nothing)) (fmap ams)) .
  right'
{-# INLINE partialIso #-}

-- | A parsing\/building combinator: try the 'PartialIso' first in both
-- directions, falling back to the given 'Prism' when it fails.
--
-- The name echoes 'failing', which likewise prefers its first argument and
-- falls back to the second; here the fallback happens in both the matching and
-- the building directions.
--
-- @
-- 'failing'' :: 'PartialIso' s t a b -> 'Prism' s t a b -> 'Prism' s t a b
-- @
--
-- /Laws:/ @'failing'' i p@ is a lawful 'Prism' when @i@ is a lawful
-- 'PartialIso', @p@ is a lawful 'Prism', and the two agree where they overlap:
-- whenever both @i@ and @p@ match the same @s@ they must focus the same @a@,
-- and whenever both build from the same @b@ they must produce the same @t@.
-- Disjoint @i@ and @p@ satisfy this trivially.
--
-- >>> let combo = failing' intPI (_Show :: Prism' String Int)
--
-- >>> "100" ^? getting combo
-- Just 100
--
-- >>> combo # 7
-- "7"
failing'
  :: PartialIso s t a b
  -> Prism s t a b
  -> Prism s t a b
failing' i p =
  prism
  (\b -> fromMaybe (reviewing p # b) (i #? b))
  (\s -> maybe (matching p s) Right (s ^? getting i))
{-# INLINE failing' #-}
