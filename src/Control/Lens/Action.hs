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
  , performs
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
type Action m s a = forall f r. Effective m r f => (a -> f a) -> s -> f s

-- | A 'MonadicFold' is a 'Fold' enriched with access to a 'Monad' for side-effects.
--
-- Every 'Fold' can be used as a 'MonadicFold', that simply ignores the access to the 'Monad'.
--
-- You can compose a 'MonadicFold' with another 'MonadicFold' using ('Prelude..') from the @Prelude@.
type MonadicFold m s a = forall f r. (Effective m r f, Applicative f) => (a -> f a) -> s -> f s

-- | Used to evaluate an 'Action'.
type Acting m r s t a b = (a -> Effect m r b) -> s -> Effect m r t

-- | Perform an 'Action'.
--
-- > perform = flip (^!)
perform :: Monad m => Acting m a s t a b -> s -> m a
perform l = getEffect . l (Effect . return)
{-# INLINE perform #-}

-- | Perform an 'Action' and modify the result.
performs :: Monad m => Acting m e s t a b -> (a -> e) -> s -> m e
performs l f = getEffect . l (Effect . return . f)

-- | Perform an 'Action'
--
-- >>> ["hello","world"]^!folded.act putStrLn
-- hello
-- world
(^!) :: Monad m => s -> Acting m a s t a b -> m a
a ^! l = getEffect (l (Effect . return) a)
{-# INLINE (^!) #-}

-- | Construct an 'Action' from a monadic side-effect
act :: Monad m => (s -> m a) -> Action m s a
act sma afb a = effective (sma a >>= ineffective . afb)
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
liftAct :: (MonadTrans trans, Monad m) => Acting m a s t a b -> Action (trans m) s a
liftAct l = act (lift . perform l)
{-# INLINE liftAct #-}
