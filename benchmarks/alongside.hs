{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Lens
import Criterion.Main
import Data.Functor.Compose
import Data.Functor.Identity

-- | A finally encoded Store
newtype Experiment c d a = Experiment { runExperiment :: forall f. Functor f => (c -> f d) -> f a }

instance Functor (Experiment c d) where
  fmap f (Experiment k) = Experiment (fmap f . k)

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

instance (c ~ d) => ComonadStore c (Experiment c d) where
  pos m = getConst (runExperiment m Const)
  peek d m = runIdentity $ runExperiment m (\_ -> Identity d)
  peeks f m = runIdentity $ runExperiment m (\c -> Identity (f c))
  experiment cfd m = runExperiment m cfd

compound :: Lens a b c d
         -> Lens a' b' c' d'
         -> Lens (a,a') (b,b') (c,c') (d,d')
compound l r = lens (\(a, a') -> (view l a, view r a'))
                    (\(a, a') (b, b') -> (set l b a, set r b' a'))
{-# INLINE compound #-}

-- BWAHAAHA
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
    , bench "compound1"  $ nf (view $ compound _1 _2) (("hi", 1), (2, "there!"))
    , bench "alongside5"  $ nf (view $ (alongside _1 (alongside _1 (alongside _1 (alongside _1 _1)))))
      ((v,v),((v,v),((v,v),((v, v),(v,v)))))
    , bench "compound5"  $ nf (view $ compound5 _1 _1 _1 _1 _1)
        ((v,v),((v,v),((v,v),((v, v),(v,v)))))
    ]
  where v = 1 :: Int
