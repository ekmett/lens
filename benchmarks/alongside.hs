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
newtype Experiment c d a = Experiment { runExperiment :: forall f. Functor f => (c -> f d) -> f a }

instance Functor (Experiment c d) where
  fmap f (Experiment k) = Experiment (fmap f . k)
  {-# INLINE fmap #-}

instance (c ~ d) => Comonad (Experiment c d) where
  extract (Experiment m) = runIdentity (m Identity)
  {-# INLINE extract #-}
  duplicate = duplicateExperiment
  {-# INLINE duplicate #-}

-- | 'Experiment' is an indexed 'Comonad'.
duplicateExperiment :: Experiment c e a -> Experiment c d (Experiment d e a)
duplicateExperiment (Experiment m) = getCompose (m (Compose . fmap placebo . placebo))
{-# INLINE duplicateExperiment #-}

-- | A trivial 'Experiment'.
placebo :: c -> Experiment c d d
placebo i = Experiment (\k -> k i)
{-# INLINE placebo #-}

instance (c ~ d) => ComonadStore c (Experiment c d) where
  pos m = posExperiment m
  peek d m = peekExperiment d m
  peeks f m = runIdentity $ runExperiment m (\c -> Identity (f c))
  experiment f m = runExperiment m f

posExperiment :: Experiment c d a -> c
posExperiment m = getConst (runExperiment m Const)
{-# INLINE posExperiment #-}

peekExperiment :: d -> Experiment c d a -> a
peekExperiment d m = runIdentity $ runExperiment m (\_ -> Identity d)
{-# INLINE peekExperiment #-}

trial :: Lens a b c d -> Lens a' b' c' d' -> Lens (a,a') (b,b') (c,c') (d,d')
trial l r pfq (a,a') = fmap (\(d,b') -> (peekExperiment d x,b')) (getCompose (r (\c' -> Compose $ pfq (posExperiment x, c')) a'))
  where x = l placebo a
{-# INLINE trial #-}

posContext :: Context c d a -> c
posContext (Context _ c) = c
{-# INLINE posContext #-}

peekContext :: d -> Context c d a -> a
peekContext d (Context f _) = f d
{-# INLINE peekContext #-}

-- a version of alongside built with Context and product
half :: LensLike (Context c d) a b c d -> Lens a' b' c' d' -> Lens (a,a') (b,b') (c,c') (d,d')
half l r pfq (a,a') = fmap (\(d,b') -> (peekContext d x,b')) (getCompose (r (\c' -> Compose $ pfq (posContext x, c')) a'))
  where x = l (Context id) a
{-# INLINE half #-}

-- alongside' :: Lens a b c d -> Lens a' b' c' d' -> Lens (a,a') (b,b') (c,c') (d,d')
-- {-# INLINE alongside'#-}

compound :: Lens a b c d
         -> Lens a' b' c' d'
         -> Lens (a,a') (b,b') (c,c') (d,d')
compound l r = lens (\(a, a') -> (view l a, view r a'))
                    (\(a, a') (b, b') -> (set l b a, set r b' a'))
{-# INLINE compound #-}

compound5 :: Lens a b c d
          -> Lens a' b' c' d'
          -> Lens a'' b'' c'' d''
          -> Lens a''' b''' c''' d'''
          -> Lens a'''' b'''' c'''' d''''
          -> Lens (a, (a', (a'', (a''', a''''))))
                  (b, (b', (b'', (b''', b''''))))
                  (c, (c', (c'', (c''', c''''))))
                  (d, (d', (d'', (d''', d''''))))
compound5 l l' l'' l''' l''''
  = lens (\(a, (a', (a'', (a''', a''''))))
           -> (view l a, (view l' a', (view l'' a'', (view l''' a''', view l'''' a'''')))) )
         (\(a, (a', (a'', (a''', a'''')))) (b, (b', (b'', (b''', b''''))))
           -> (set l b a, (set l' b' a', (set l'' b'' a'', (set l''' b''' a''', set l'''' b'''' a'''')))) )

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
