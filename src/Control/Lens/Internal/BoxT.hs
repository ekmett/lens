{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
module Control.Lens.Internal.BoxT where
import Control.Applicative
import Data.Functor.Apply (Apply (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Tuple.Solo (Solo (..))

-- | A very simple applicative transformer that gives us more control over when
-- things get forced. Note: this type /should not/ be made an instance of
-- @Settable@, because then users could accidentally use
-- 'Control.Lens.Traversable.strictly' with a 'Control.Lens.Setter.Setter',
-- which will not work at all. There is no way to strictify a @Setter@.
newtype BoxT f a = BoxT
  { runBoxT :: f (Solo a) }
  deriving (Functor, Foldable, Traversable)

-- The Contravariant instance allows `strictly` to be used on a getter or fold.
-- It's not at all obvious that this is *useful* (since `strictly` doesn't
-- change these at all), but it's also not obviously *harmful*.
instance Contravariant f => Contravariant (BoxT f) where
  contramap f (BoxT m) = BoxT $ contramap (fmap f) m
  {-# INLINE contramap #-}
instance Apply f => Apply (BoxT f) where
  BoxT m <.> BoxT n = BoxT (liftF2 (<*>) m n)
#if MIN_VERSION_semigroupoids(5,3,0)
  liftF2 f (BoxT m) (BoxT n) = BoxT (liftF2 (liftA2 f) m n)
  {-# INLINE liftF2 #-}
#endif
instance Applicative f => Applicative (BoxT f) where
  pure = BoxT . pure . Solo
  {-# INLINE pure #-}
  BoxT m <*> BoxT n = BoxT (liftA2 (<*>) m n)
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 f (BoxT m) (BoxT n) = BoxT (liftA2 (liftA2 f) m n)
  {-# INLINE liftA2 #-}
#endif
  -- Caution: We *can't* implement *> or <* in terms of the underlying *> and
  -- <*. We need to force the Solos, not discard them.
