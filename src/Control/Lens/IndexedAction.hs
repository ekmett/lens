{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.IndexedAction
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.IndexedAction
  (
  -- * Composable Actions
    IndexedAction
  , iact
  , iperform
  , iperforms
  , (^@!)

  -- * Folds with Effects
  , IndexedMonadicFold

  -- * Implementation Details
  , IndexedActing
  ) where

import Control.Applicative
import Control.Lens.Classes
import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Lens.Internal.Composition

-- $setup
-- >>> :m + Control.Lens

infixr 8 ^@!

-- | An 'IndexedAction' is an 'IndexedGetter' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Getter' can be used as an 'Action'
--
-- You can compose an 'Action' with another 'Action' using ('Prelude..') from the @Prelude@.
type IndexedAction i m s a = forall p f r. (Indexable i p, Effective m r f) => p a (f a) -> s -> f s

-- | An 'IndexedMonadicFold' is an 'IndexedFold' enriched with access to a 'Monad' for side-effects.
--
-- Every 'IndexedFold' can be used as an 'IndexedMonadicFold', that simply ignores the access to the 'Monad'.
--
-- You can compose an 'IndexedMonadicFold' with another 'IndexedMonadicFold' using ('Prelude..') from the @Prelude@.
type IndexedMonadicFold i m s a = forall p f r. (Indexable i p, Effective m r f, Applicative f) => p a (f a) -> s -> f s

-- | Used to evaluate an 'IndexedAction'.
type IndexedActing i m r s t a b = Indexed i a (Effect m r b) -> s -> Effect m r t

-- | Perform an 'IndexedAction'.
--
-- @'perform' ≡ 'flip' ('^@!')@
iperform :: Monad m => IndexedActing i m (i, a) s t a b -> s -> m (i, a)
iperform l = getEffect #. withIndex l (\i a -> Effect (return (i, a)))
{-# INLINE iperform #-}

-- | Perform an 'IndexedAction' and modify the result.
iperforms :: Monad m => IndexedActing i m e s t a b -> (i -> a -> e) -> s -> m e
iperforms l f = getEffect #. withIndex l (\i a -> Effect (return (f i a)))

-- | Perform an 'IndexedAction'
--
-- >>> ["hello","world"]^!traversed.iact putStrLn
-- (0,hello)
-- (1,world)
(^@!) :: Monad m => s -> IndexedActing i m (i, a) s t a b -> m (i, a)
s ^@! l = getEffect (withIndex l (\i a -> Effect (return (i, a))) s)
{-# INLINE (^@!) #-}

-- | Construct an 'IndexedAction' from a monadic side-effect
iact :: Monad m => (s -> m (i, a)) -> IndexedAction i m s a
iact sma iafb a = effective (sma a >>= ineffective . uncurry (indexed iafb))
{-# INLINE iact #-}
