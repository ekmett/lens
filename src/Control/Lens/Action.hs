{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Action
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MTPCs, FDs, Rank2
--
----------------------------------------------------------------------------
module Control.Lens.Action
  (
  -- * Composable Actions
    Action
  , act
  , acts
  , perform
  , liftAct
  , (^!)

  -- * Folds with Effecs
  , MonadicFold

  -- * Implementation Details
  , Acting
  ) where

import Control.Applicative
import Control.Lens.Internal
import Control.Monad.Trans.Class

-- $setup
-- >>> import Control.Lens

infixr 8 ^!

-- | An 'Action' is a 'Getter' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Getter' can be used as an 'Action'
--
-- You can compose an 'Action' with another 'Action' using ('Prelude..') from the @Prelude@.
type Action m a c = forall f r. Effective m r f => (c -> f c) -> a -> f a

-- | A 'MonadicFold' is a 'Fold' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Fold' can be used as a 'MonadicFold', that simply ignores the access to the 'Monad'.
--
-- You can compose a 'MonadicFold' with another 'MonadicFold' using ('Prelude..') from the @Prelude@.
type MonadicFold m a c = forall f r. (Effective m r f, Applicative f) => (c -> f c) -> a -> f a

-- | Used to evaluate an 'Action'.
type Acting m r a b c d = (c -> Effect m r d) -> a -> Effect m r b

-- | Perform an 'Action'.
--
-- > perform = flip (^!)
perform :: Monad m => Acting m c a b c d -> a -> m c
perform l = getEffect . l (Effect . return)
{-# INLINE perform #-}

-- | Perform an 'Action'
--
-- >>> ["hello","world"]^!folded.act putStrLn
-- hello
-- world
(^!) :: Monad m => a -> Acting m c a b c d -> m c
a ^! l = getEffect (l (Effect . return) a)
{-# INLINE (^!) #-}

-- | Construct an 'Action' from a monadic side-effect
act :: Monad m => (a -> m c) -> Action m a c
act amc cfd a = effective (amc a >>= ineffective . cfd)
{-# INLINE act #-}

-- | A self-running 'Action', analogous to 'Control.Monad.join'.
--
-- @'acts' = 'act' 'id'@
--
-- >>> (1,"hello")^!_2.acts.to succ
-- "ifmmp"
acts :: Action m (m a) a
acts = act id
{-# INLINE acts #-}

-- | Apply a 'Monad' transformer to an 'Action'.
liftAct :: (MonadTrans t, Monad m) => Acting m c a b c d -> Action (t m) a c
liftAct l = act (lift . perform l)
{-# INLINE liftAct #-}
