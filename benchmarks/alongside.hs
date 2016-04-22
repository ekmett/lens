{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Lens.Internal
import Control.Lens
import Criterion.Main
import Data.Functor.Compose
import Data.Functor.Identity

-- | A finally encoded Store
newtype Experiment a b s = Experiment { runExperiment :: forall f. Functor f => (a -> f b) -> f s }

instance Functor (Experiment a b) where
  fmap f (Experiment k) = Experiment (fmap f . k)
  {-# INLINE fmap #-}

instance (a ~ b) => Comonad (Experiment a b) where
  extract (Experiment m) = runIdentity (m Identity)
  {-# INLINE extract #-}
  duplicate = duplicateExperiment
  {-# INLINE duplicate #-}

-- | 'Experiment' is an indexed 'Comonad'.
duplicateExperiment :: Experiment a c s -> Experiment a b (Experiment b c s)
duplicateExperiment (Experiment m) = getCompose (m (Compose . fmap placebo . placebo))
{-# INLINE duplicateExperiment #-}

-- | A trivial 'Experiment'.
placebo :: a -> Experiment a b b
placebo i = Experiment (\k -> k i)
{-# INLINE placebo #-}

instance (a ~ b) => ComonadStore a (Experiment a b) where
  pos m = posExperiment m
  peek d m = peekExperiment d m
  peeks f m = runIdentity $ runExperiment m (\c -> Identity (f c))
  experiment f m = runExperiment m f

posExperiment :: Experiment a b s -> a
posExperiment m = getConst (runExperiment m Const)
{-# INLINE posExperiment #-}

peekExperiment :: b -> Experiment a b s -> s
peekExperiment b m = runIdentity $ runExperiment m (\_ -> Identity b)
{-# INLINE peekExperiment #-}

trial :: Lens s t a b -> Lens s' t' a' b' -> Lens (s,s') (t,t') (a,a') (b,b')
trial l r pfq (s,s') = fmap (\(b,t') -> (peekExperiment b x,t')) (getCompose (r (\a' -> Compose $ pfq (posExperiment x, a')) s'))
  where x = l placebo s
{-# INLINE trial #-}

posContext :: Context a b s -> a
posContext (Context _ a) = a
{-# INLINE posContext #-}

peekContext :: b -> Context a b s -> s
peekContext b (Context f _) = f b
{-# INLINE peekContext #-}

-- a version of alongside built with Context and product
half :: LensLike (Context a b) s t a b -> Lens s' t' a' b' -> Lens (s,s') (t,t') (a,a') (b,b')
half l r pfq (s,s') = fmap (\(b,t') -> (peekContext b x,t')) (getCompose (r (\a' -> Compose $ pfq (posContext x, a')) s'))
  where x = l (Context id) s
{-# INLINE half #-}

-- alongside' :: Lens s t a b -> Lens s' t' a' b' -> Lens (s,s') (t,t') (a,a') (b,b')
-- {-# INLINE alongside'#-}

compound :: Lens' s a
         -> Lens' s' a'
         -> Lens' (s,s') (a,a')
compound l r = lens (\(s, s') -> (view l s, view r s'))
                    (\(s, s') (t, t') -> (set l t s, set r t' s'))
{-# INLINE compound #-}

compound5 :: Lens' s a
          -> Lens' s' a'
          -> Lens' s'' a''
          -> Lens' s''' a'''
          -> Lens' s'''' a''''
          -> Lens' (s, (s', (s'', (s''', s''''))))
                  (a, (a', (a'', (a''', a''''))))
compound5 l l' l'' l''' l''''
  = lens (\(s, (s', (s'', (s''', s''''))))
           -> (view l s, (view l' s', (view l'' s'', (view l''' s''', view l'''' s'''')))) )
         (\(s, (s', (s'', (s''', s'''')))) (t, (t', (t'', (t''', t''''))))
           -> (set l t s, (set l' t' s', (set l'' t'' s'', (set l''' t''' s''', set l'''' t'''' s'''')))) )

main = defaultMain
    [ bench "alongside1" $ nf (view $ alongside _1 _2) (("hi", 1), (2, "there!"))
    , bench "trial1" $ nf (view $ trial _1 _2) (("hi", 1), (2, "there!"))
    , bench "half1" $ nf (view $ half _1 _2) (("hi", 1), (2, "there!"))
    , bench "compound1"  $ nf (view $ compound _1 _2) (("hi", 1), (2, "there!"))
    , bench "alongside5"  $ nf (view $ (alongside _1 (alongside _1 (alongside _1 (alongside _1 _1)))))
      ((v,v),((v,v),((v,v),((v,v),(v,v)))))
    , bench "trial5"  $ nf (view $ (trial _1 (trial _1 (trial _1 (trial _1 _1)))))
      ((v,v),((v,v),((v,v),((v,v),(v,v)))))
    , bench "half5"  $ nf (view $ (half _1 (half _1 (half _1 (half _1 _1)))))
      ((v,v),((v,v),((v,v),((v,v),(v,v)))))
    , bench "compound5"  $ nf (view $ compound5 _1 _1 _1 _1 _1)
      ((v,v),((v,v),((v,v),((v,v),(v,v)))))
    ]
  where v = 1 :: Int
