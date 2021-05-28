{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A collection of properties that can be tested with QuickCheck, to guarantee
-- that you are working with valid 'Lens'es, 'Setter's, 'Traversal's, 'Iso's and
-- 'Prism's.
module Control.Lens.Properties
    ( isLens
    , isTraversal
    , isSetter
    , isIso
    , isPrism
    ) where

import Control.Lens
import Data.Functor.Compose
import Test.QuickCheck

--------------------------------------------------------------------------------
-- | A 'Setter' is only legal if the following 3 laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
isSetter :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Setter' s a -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l


--------------------------------------------------------------------------------
-- | A 'Traversal' is only legal if it is a valid 'Setter' (see 'isSetter' for
-- what makes a 'Setter' valid), and the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
isTraversal :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Traversal' s a -> Property
isTraversal l = isSetter l .&. traverse_pureMaybe l .&. traverse_pureList l
                  .&. do as <- arbitrary
                         bs <- arbitrary
                         t <- arbitrary
                         return $ traverse_compose l (\x -> as++[x]++bs)
                                                     (\x -> if t then Just x else Nothing)


--------------------------------------------------------------------------------
-- | A 'Lens' is only legal if it is a valid 'Traversal' (see 'isTraversal' for
-- what this means), and if the following laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
isLens :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
       => Lens' s a -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l


--------------------------------------------------------------------------------
isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
      => Iso' s a -> Property
isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)


--------------------------------------------------------------------------------
isPrism :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
      => Prism' s a -> Property
isPrism l = isTraversal l .&. prism_yin l .&. prism_yang l


--------------------------------------------------------------------------------
-- The first setter law:
setter_id :: Eq s => Setter' s a -> s -> Bool
setter_id l s = over l id s == s

--  The second setter law:
setter_composition :: Eq s => Setter' s a -> s -> Fun a a -> Fun a a -> Bool
setter_composition l s (Fun _ f) (Fun _ g) = over l f (over l g s) == over l (f . g) s

lens_set_view :: Eq s => Lens' s a -> s -> Bool
lens_set_view l s = set l (view l s) s == s

lens_view_set :: Eq a => Lens' s a -> s -> a -> Bool
lens_view_set l s a = view l (set l a s) == a

setter_set_set :: Eq s => Setter' s a -> s -> a -> a -> Bool
setter_set_set l s a b = set l b (set l a s) == set l b s

iso_hither :: Eq s => AnIso' s a -> s -> Bool
iso_hither l s = s ^.cloneIso l.from l == s

iso_yon :: Eq a => AnIso' s a -> a -> Bool
iso_yon l a = a^.from l.cloneIso l == a

prism_yin :: Eq a => Prism' s a -> a -> Bool
prism_yin l a = preview l (review l a) == Just a

prism_yang :: Eq s => Prism' s a -> s -> Bool
prism_yang l s = maybe s (review l) (preview l s) == s

traverse_pure :: forall f s a. (Applicative f, Eq (f s)) => LensLike' f s a -> s -> Bool
traverse_pure l s = l pure s == (pure s :: f s)

traverse_pureMaybe :: Eq s => LensLike' Maybe s a -> s -> Bool
traverse_pureMaybe = traverse_pure

traverse_pureList :: Eq s => LensLike' [] s a -> s -> Bool
traverse_pureList = traverse_pure

traverse_compose :: (Applicative f, Applicative g, Eq (f (g s)))
                    => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s
