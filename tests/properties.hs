{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Functor.Identity
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Data.Text.Strict.Lens
import Data.List.Lens

setter_id :: Eq s => Simple Setter s a -> s -> Bool
setter_id l s = runIdentity (l Identity s) == s

setter_composition :: Eq s => Simple Setter s a -> s -> Fun a a -> Fun a a -> Bool
setter_composition l s (Fun _ f) (Fun _ g) = over l f (over l g s) == over l (f . g) s

lens_set_view :: Eq s => Simple Lens s a -> s -> Bool
lens_set_view l s = set l (view l s) s == s

lens_view_set :: Eq a => Simple Lens s a -> s -> a -> Bool
lens_view_set l s a = view l (set l a s) == a

setter_set_set :: Eq s => Simple Setter s a -> s -> a -> a -> Bool
setter_set_set l s a b = set l b (set l a s) == set l b s

iso_hither :: Eq s => Simple AnIso s a -> s -> Bool
iso_hither l s = s ^.cloneIso l.from l == s

iso_yon :: Eq a => Simple AnIso s a -> a -> Bool
iso_yon l a = a^.from l.cloneIso l == a

prism_yen :: Eq a => Simple APrism s a -> a -> Bool
prism_yen l a = a^.remit l^?clonePrism l == Just a

traverse_pure :: forall f s a. (Applicative f, Eq (f s)) => SimpleLensLike f s a -> s -> Bool
traverse_pure l s = l pure s == (pure s :: f s)

traverse_pureMaybe :: Eq s => SimpleLensLike Maybe s a -> s -> Bool
traverse_pureMaybe = traverse_pure

traverse_pureList :: Eq s => SimpleLensLike [] s a -> s -> Bool
traverse_pureList = traverse_pure

isSetter :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Simple Setter s a -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l

isTraversal :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Simple Traversal s a -> Property
isTraversal l = isSetter l .&. traverse_pureMaybe l .&. traverse_pureList l

isLens :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
       => Simple Lens s a -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
      => Simple Iso s a -> Property
isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)

isPrism :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
      => Simple Prism s a -> Property
isPrism l = isTraversal l .&. prism_yen l

-- an illegal lens
bad :: Simple Lens (Int,Int) Int
bad f (a,b) = (,) b <$> f a

badIso :: Simple Iso Int Bool
badIso = iso even fromEnum

-- Control.Lens.Type
prop_1                               = isLens (_1 :: Simple Lens (Int,Double,()) Int)
prop_2                               = isLens (_2 :: Simple Lens (Int,Bool) Bool)
prop_3                               = isLens (_3 :: Simple Lens (Int,Bool,()) ())
prop_4                               = isLens (_4 :: Simple Lens (Int,Bool,(),Maybe Int) (Maybe Int))
prop_5                               = isLens (_5 :: Simple Lens ((),(),(),(),Int) Int)

prop_2_2                             = isLens (_2._2 :: Simple Lens (Int,(Int,Bool),Double) Bool)

-- prop_illegal_lens                    = expectFailure $ isLens bad
-- prop_illegal_traversal               = expectFailure $ isTraversal bad
-- prop_illegal_setter                  = expectFailure $ isSetter bad
-- prop_illegal_iso                     = expectFailure $ isIso badIso

-- Control.Lens.Setter
prop_mapped                          = isSetter (mapped :: Simple Setter [Int] Int)
prop_mapped_mapped                   = isSetter (mapped.mapped :: Simple Setter [Maybe Int] Int)

prop_both                            = isTraversal (both    :: Simple Traversal (Int,Int) Int)
prop_value (Fun _ k :: Fun Int Bool) = isTraversal (value k :: Simple Traversal (Int,Int) Int)
prop_traverseLeft                    = isTraversal (_left   :: Simple Traversal (Either Int Bool) Int)
prop_traverseRight                   = isTraversal (_right  :: Simple Traversal (Either Int Bool) Bool)

prop_simple                          = isIso (simple :: Simple Iso Int Int)
--prop_enum                            = isIso (enum :: Simple Iso Int Char)

prop__left                           = isPrism (_left :: Simple Prism (Either Int Bool) Int)
prop__right                          = isPrism (_right :: Simple Prism (Either Int Bool) Bool)
prop__just                           = isPrism (_just :: Simple Prism (Maybe Int) Int)

-- Data.List.Lens
prop_strippingPrefix s               = isPrism (strippingPrefix s :: Simple Prism String String)

-- Data.Text.Lens
prop_text s                          = s^.packed.from packed == s
--prop_text                           = isIso packed

main :: IO ()
main = do
  b <- $quickCheckAll
  unless b $ exitWith (ExitFailure 1)
