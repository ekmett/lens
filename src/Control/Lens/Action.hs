{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Comonad
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Action
import Control.Lens.Type
import Control.Monad.Trans.Class
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Unsafe

-- $setup
-- >>> :m + Control.Lens

infixr 8 ^!, ^@!

-- | Used to evaluate an 'Action'.
type Acting m r s t a b = (a -> Effect m r b) -> s -> Effect m r t

-- | Perform an 'Action'.
--
-- @'perform' ≡ 'flip' ('^!')@
perform :: Monad m => Acting m a s t a b -> s -> m a
perform l = getEffect #. l (Effect #. return)
{-# INLINE perform #-}

-- | Perform an 'Action' and modify the result.
--
-- @'performs' :: 'Monad' m => 'Acting' m e s t a b -> (a -> e) -> s -> m e@
performs :: (Profunctor p, Monad m) => Overloading p (->) (Effect m e) s t a b -> p a e -> s -> m e
performs l f = getEffect #. l (rmap (Effect #. return) f)
{-# INLINE performs #-}

-- | Perform an 'Action'.
--
-- >>> ["hello","world"]^!folded.act putStrLn
-- hello
-- world
(^!) :: Monad m => s -> Acting m a s t a b -> m a
a ^! l = getEffect (l (Effect #. return) a)
{-# INLINE (^!) #-}

-- | Construct an 'Action' from a monadic side-effect.
--
-- >>> ["hello","world"]^!folded.act (\x -> [x,x ++ "!"])
-- ["helloworld","helloworld!","hello!world","hello!world!"]
--
-- @
-- 'act' :: 'Monad' m => (s -> m a) -> 'Action' m s a
-- 'act' sma afb a = 'effective' (sma a '>>=' 'ineffective' '.' afb)
-- @
act :: Monad m => (s -> m a) -> IndexPreservingAction m s a
act sma pafb = cotabulate $ \ws -> effective $ do
   a <- sma (extract ws)
   ineffective (corep pafb (a <$ ws))
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

-- | Used to evaluate an 'IndexedAction'.
type IndexedActing i m r s t a b = Indexed i a (Effect m r b) -> s -> Effect m r t

-- | Perform an 'IndexedAction'.
--
-- @'perform' ≡ 'flip' ('^@!')@
iperform :: Monad m => IndexedActing i m (i, a) s t a b -> s -> m (i, a)
iperform l = getEffect #. l (Indexed $ \i a -> Effect (return (i, a)))
{-# INLINE iperform #-}

-- | Perform an 'IndexedAction' and modify the result.
iperforms :: Monad m => IndexedActing i m e s t a b -> (i -> a -> e) -> s -> m e
iperforms l = performs l .# Indexed
{-# INLINE iperforms #-}

-- | Perform an 'IndexedAction'.
(^@!) :: Monad m => s -> IndexedActing i m (i, a) s t a b -> m (i, a)
s ^@! l = getEffect (l (Indexed $ \i a -> Effect (return (i, a))) s)
{-# INLINE (^@!) #-}

-- | Construct an 'IndexedAction' from a monadic side-effect.
iact :: Monad m => (s -> m (i, a)) -> IndexedAction i m s a
iact smia iafb s = effective $ do
  (i, a) <- smia s
  ineffective (indexed iafb i a)
{-# INLINE iact #-}
