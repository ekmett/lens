{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Action
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
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

  -- * Indexed Actions
  , IndexedAction
  , iact
  , iperform
  , iperforms
  , (^@!)

  -- * Folds with Effects
  , MonadicFold
  , IndexedMonadicFold

  -- * Implementation Details
  , Acting
  , IndexedActing
  , Effective
  ) where

import Control.Applicative
import Control.Lens.Classes
import Control.Lens.Internal
import Control.Lens.Internal.Composition
import Control.Lens.Lens
import Control.Monad.Trans.Class

-- $setup
-- >>> :m + Control.Lens

infixr 8 ^!, ^@!

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
-- @'perform' ≡ 'flip' ('^!')@
perform :: Monad m => Acting m a s t a b -> s -> m a
perform l = getEffect #. l (Effect #. return)
{-# INLINE perform #-}

-- | Perform an 'Action' and modify the result.
performs :: Monad m => Acting m e s t a b -> (a -> e) -> s -> m e
performs l f = getEffect #. l (Effect #. return . f)

-- | Perform an 'Action'
--
-- >>> ["hello","world"]^!folded.act putStrLn
-- hello
-- world
(^!) :: Monad m => s -> Acting m a s t a b -> m a
a ^! l = getEffect (l (Effect #. return) a)
{-# INLINE (^!) #-}

-- | Construct an 'Action' from a monadic side-effect
--
-- >>> ["hello","world"]^!folded.act (\x -> [x,x ++ "!"])
-- ["helloworld","helloworld!","hello!world","hello!world!"]
act :: Monad m => (s -> m a) -> Action m s a
act sma afb a = effective (sma a >>= ineffective . afb)
{-# INLINE act #-}

-- | A self-running 'Action', analogous to 'Control.Monad.join'.
--
-- @'acts' ≡ 'act' 'id'@
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

-----------------------------------------------------------------------------
-- Indexed Actions
----------------------------------------------------------------------------

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
