-- |
-- Module      :  Control.Applicative.Lift
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Adding a new kind of pure computation to an applicative functor.
--
-- NB: This module is only included in @lens@ for backwards compatibility with
-- @transformers@ versions before 3.0.

module Control.Applicative.Lift (
    Lift(..), unLift,
    -- * Collecting errors
    Errors, failure
  ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Constant
import Data.Monoid
import Data.Traversable (Traversable(traverse))

-- | Applicative functor formed by adding pure computations to a given
-- applicative functor.
data Lift f a = Pure a | Other (f a)

instance (Functor f) => Functor (Lift f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Other y) = Other (fmap f y)

instance (Foldable f) => Foldable (Lift f) where
    foldMap f (Pure x) = f x
    foldMap f (Other y) = foldMap f y

instance (Traversable f) => Traversable (Lift f) where
    traverse f (Pure x) = Pure <$> f x
    traverse f (Other y) = Other <$> traverse f y

-- | A combination is 'Pure' only if both parts are.
instance (Applicative f) => Applicative (Lift f) where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Other y = Other (f <$> y)
    Other f <*> Pure x = Other (($ x) <$> f)
    Other f <*> Other y = Other (f <*> y)

-- | A combination is 'Pure' only either part is.
instance Alternative f => Alternative (Lift f) where
    empty = Other empty
    Pure x <|> _ = Pure x
    Other _ <|> Pure y = Pure y
    Other x <|> Other y = Other (x <|> y)

-- | Projection to the other functor.
unLift :: Applicative f => Lift f a -> f a
unLift (Pure x) = pure x
unLift (Other e) = e

-- | An applicative functor that collects a monoid (e.g. lists) of errors.
-- A sequence of computations fails if any of its components do, but
-- unlike monads made with 'ErrorT' from "Control.Monad.Trans.Error",
-- these computations continue after an error, collecting all the errors.
type Errors e = Lift (Constant e)

-- | Report an error.
failure :: Monoid e => e -> Errors e a
failure e = Other (Constant e)
