{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif


-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zoom
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-------------------------------------------------------------------------------
module Control.Lens.Zoom
  ( Magnify(..)
  , Zoom(..)
  ) where

import Control.Lens.Internal
import Control.Lens.Type
import Control.Lens.Getter
import Control.Monad
import Control.Monad.Reader.Class       as Reader
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy   as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy   as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Monoid

-- | This class allows us to use 'zoom' in, changing the State supplied by
-- many different monad transformers, potentially quite deep in a monad transformer stack.
class (MonadState s m, MonadState t n) => Zoom m n k s t | m -> s k, n -> t k, m t -> n, n s -> m where
  -- | Run a monadic action in a larger state than it was defined in,
  -- using a 'Simple' 'Lens' or 'Simple' 'Control.Lens.Traversal.Traversal'.
  --
  -- This is commonly used to lift actions in a simpler state monad into a
  -- state monad with a larger state type.
  --
  -- When applied to a 'Simple 'Control.Lens.Traversal.Traversal' over
  -- multiple values, the actions for each target are executed sequentially
  -- and the results are aggregated.
  --
  -- This can be used to edit pretty much any monad transformer stack with a state in it!
  --
  -- @
  -- 'zoom' :: 'Monad' m             => 'Simple' 'Lens' s t      -> 'StateT' t m a -> 'StateT' s m a
  -- 'zoom' :: ('Monad' m, 'Monoid' c) => 'Simple' 'Control.Lens.Traversal.Traversal' s t -> 'StateT' t m c -> 'StateT' s m c
  -- 'zoom' :: 'Monad' m             => 'Simple' 'Lens' s t      -> 'RWST' r w t m c -> 'RWST' r w s m c
  -- 'zoom' :: ('Monad' m, 'Monoid' c) => 'Simple' 'Control.Lens.Traversal.Traversal' s t -> 'RWST' r w t m c -> 'RWST' r w s m c
  -- 'zoom' :: 'Monad' m             => 'Simple' 'Lens' s t      -> 'ErrorT' e ('RWST' r w t m c) -> 'ErrorT' e ('RWST' r w s m c)
  -- 'zoom' :: ('Monad' m, 'Monoid' c) => 'Simple' 'Control.Lens.Traversal.Traversal' s t -> 'ErrorT' e ('RWST' r w t m c) -> 'ErrorT' e ('RWST' r w s m c)
  -- ...
  -- @
  zoom :: Monad m => SimpleLensLike (k c) t s -> m c -> n c

instance Monad z => Zoom (Strict.StateT s z) (Strict.StateT t z) (Focusing z) s t where
  zoom l (Strict.StateT m) = Strict.StateT $ unfocusing . l (Focusing . m)
  {-# INLINE zoom #-}

instance Monad z => Zoom (Lazy.StateT s z) (Lazy.StateT t z) (Focusing z) s t where
  zoom l (Lazy.StateT m) = Lazy.StateT $ unfocusing . l (Focusing . m)
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (ReaderT e m) (ReaderT e n) k s t where
  zoom l (ReaderT m) = ReaderT (zoom l . m)
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (IdentityT m) (IdentityT n) k s t where
  zoom l (IdentityT m) = IdentityT (zoom l m)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Strict.RWST r w s z) (Strict.RWST r w t z) (FocusingWith w z) s t where
  zoom l (Strict.RWST m) = Strict.RWST $ \r -> unfocusingWith . l (FocusingWith . m r)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Lazy.RWST r w s z) (Lazy.RWST r w t z) (FocusingWith w z) s t where
  zoom l (Lazy.RWST m) = Lazy.RWST $ \r -> unfocusingWith . l (FocusingWith . m r)
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n k s t) => Zoom (Strict.WriterT w m) (Strict.WriterT w n) (FocusingPlus w k) s t where
  zoom l = Strict.WriterT . zoom (\cfd -> unfocusingPlus . l (FocusingPlus  . cfd)) . Strict.runWriterT
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n k s t) => Zoom (Lazy.WriterT w m) (Lazy.WriterT w n) (FocusingPlus w k) s t where
  zoom l = Lazy.WriterT . zoom (\cfd -> unfocusingPlus . l (FocusingPlus  . cfd)) . Lazy.runWriterT
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (ListT m) (ListT n) (FocusingOn [] k) s t where
  zoom l = ListT . zoom (\cfd -> unfocusingOn . l (FocusingOn . cfd)) . runListT
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (MaybeT m) (MaybeT n) (FocusingMay k) s t where
  zoom l = MaybeT . liftM getMay . zoom (\cfd -> unfocusingMay . l (FocusingMay . cfd)) . liftM May . runMaybeT
  {-# INLINE zoom #-}

instance (Error e, Zoom m n k s t) => Zoom (ErrorT e m) (ErrorT e n) (FocusingErr e k) s t where
  zoom l = ErrorT . liftM getErr . zoom (\cfd -> unfocusingErr . l (FocusingErr . cfd)) . liftM Err . runErrorT
  {-# INLINE zoom #-}

-- TODO: instance Zoom m m k a a => Zoom (ContT r m) (ContT r m) k a a where


-- | This class allows us to use 'magnify' part of the environment, changing the environment supplied by
-- many different monad transformers. Unlike 'zoom' this can change the environment of a deeply nested monad transformer.
--
-- Also, unlike 'zoom', this can be used with any valid 'Getter', but cannot be used with a 'Traversal' or 'Fold'.
class (MonadReader b m, MonadReader a n) => Magnify m n k b a | m -> b, n -> a, m a -> n, n b -> m where
  -- | Run a monadic action in a larger environment than it was defined in, using a 'Getter'.
  --
  -- This acts like 'Control.Monad.Reader.Class.local', but can in many cases change the type of the environment as well.
  --
  -- This is commonly used to lift actions in a simpler Reader monad into a monad with a larger environment type.
  --
  -- This can be used to edit pretty much any monad transformer stack with an environment in it:
  --
  -- @
  -- 'magnify' ::             'Getter' s a -> (a -> r) -> s -> r
  -- 'magnify' :: 'Monoid' c => 'Fold' s a   -> (a -> r) -> s -> r
  -- 'magnify' :: 'Monoid' w                'Getter' s t -> 'RWST' s w st c -> 'RWST' t w st c
  -- 'magnify' :: ('Monoid' w, 'Monoid' c) => 'Fold' s t   -> 'RWST' s w st c -> 'RWST' t w st c
  -- ...
  -- @
  magnify :: ((b -> k c b) -> a -> k c a) -> m c -> n c

instance Monad m => Magnify (ReaderT b m) (ReaderT a m) (Effect m) b a where
  magnify l (ReaderT m) = ReaderT $ getEffect . l (Effect . m)
  {-# INLINE magnify #-}

-- | @'magnify' = 'views'@
instance Magnify ((->) b) ((->) a) Accessor b a where
  magnify = views
  {-# INLINE magnify #-}

instance (Monad m, Monoid w) => Magnify (Strict.RWST b w s m) (Strict.RWST a w s m) (EffectRWS w s m) b a where
  magnify l (Strict.RWST m) = Strict.RWST $ getEffectRWS . l (EffectRWS . m)
  {-# INLINE magnify #-}

instance (Monad m, Monoid w) => Magnify (Lazy.RWST b w s m) (Lazy.RWST a w s m) (EffectRWS w s m) b a where
  magnify l (Lazy.RWST m) = Lazy.RWST $ getEffectRWS . l (EffectRWS . m)
  {-# INLINE magnify #-}

instance Magnify m n k b a => Magnify (IdentityT m) (IdentityT n) k b a where
  magnify l (IdentityT m) = IdentityT (magnify l m)
  {-# INLINE magnify #-}
