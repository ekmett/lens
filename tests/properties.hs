{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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

setter_id :: Eq s => Simple Setter s a -> s -> Bool
setter_id l s = runIdentity (l Identity s) == s

setter_composition :: Eq s => Simple Setter s a -> s -> Fun a a -> Fun a a -> Bool
setter_composition l s (Fun _ f) (Fun _ g) = mapOf l f (mapOf l g s) == mapOf l (f . g) s

lens_set_view :: Eq s => Simple Lens s a -> s -> Bool
lens_set_view l s = set l (view l s) s == s

lens_view_set :: Eq a => Simple Lens s a -> s -> a -> Bool
lens_view_set l s a = view l (set l a s) == a

setter_set_set :: Eq s => Simple Setter s a -> s -> a -> a -> Bool
setter_set_set l s a b = set l b (set l a s) == set l b s

iso_hither :: Eq s => Simple Iso s a -> s -> Bool
iso_hither l s = s ^.l.from l == s

iso_yon :: Eq a => Simple Iso s a -> a -> Bool
iso_yon l a = a^.from l.l == a

isSetter :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Simple Setter s a -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l

isTraversal :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Simple Traversal s a -> Property
isTraversal l = isSetter l

isLens :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
       => Simple Lens s a -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
      => Simple Iso s a -> Property
isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)

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

prop_illegal_lens                    = expectFailure $ isLens bad
prop_illegal_traversal               = expectFailure $ isTraversal bad
prop_illegal_setter                  = expectFailure $ isSetter bad
prop_illegal_iso                     = expectFailure $ isIso badIso

-- Control.Lens.Setter
prop_mapped                          = isSetter (mapped :: Simple Setter [Int] Int)
prop_mapped_mapped                   = isSetter (mapped.mapped :: Simple Setter [Maybe Int] Int)

prop_both                            = isTraversal (both :: Simple Traversal (Int,Int) Int)
prop_value (Fun _ k :: Fun Int Bool) = isTraversal (value k :: Simple Traversal (Int,Int) Int)
prop_traverseLeft                    = isTraversal (traverseLeft :: Simple Traversal (Either Int Bool) Int)
prop_traverseRight                   = isTraversal (traverseRight:: Simple Traversal (Either Int Bool) Bool)

-- Data.Text.Lens
prop_text s                          = s^.packed.from packed == s

main :: IO ()
main = do
  b <- $quickCheckAll
  unless b $ exitWith (ExitFailure 1)
