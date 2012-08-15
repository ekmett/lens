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
import Data.Pair.Lens
import Data.Either.Lens
import Data.Text.Lens

setter_id :: Eq a => Simple Setter a b -> a -> Bool
setter_id l a = runIdentity (l Identity a) == a

setter_composition :: Eq a => Simple Setter a b -> a -> Fun b b -> Fun b b -> Bool
setter_composition l a (Fun _ f) (Fun _ g) = mapOf l f (mapOf l g a) == mapOf l (f . g) a

lens_set_view :: Eq a => Simple Lens a b -> a -> Bool
lens_set_view l a = set l (view l a) a == a

lens_view_set :: Eq b => Simple Lens a b -> a -> b -> Bool
lens_view_set l a b = view l (set l b a) == b

setter_set_set :: Eq a => Simple Setter a b -> a -> b -> b -> Bool
setter_set_set l a b c = set l c (set l b a) == set l c a

iso_hither :: Eq a => Simple Iso a b -> a -> Bool
iso_hither l a = a ^.l.from l == a

iso_yon :: Eq b => Simple Iso a b -> b -> Bool
iso_yon l b = b^.from l.l == b

isSetter :: (Arbitrary a, Arbitrary b, CoArbitrary b, Show a, Show b, Eq a, Function b)
         => Simple Setter a b -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l

isTraversal :: (Arbitrary a, Arbitrary b, CoArbitrary b, Show a, Show b, Eq a, Function b)
         => Simple Traversal a b -> Property
isTraversal l = isSetter l

isLens :: (Arbitrary a, Arbitrary b, CoArbitrary b, Show a, Show b, Eq a, Eq b, Function b)
       => Simple Lens a b -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isIso :: (Arbitrary a, Arbitrary b, CoArbitrary a, CoArbitrary b, Show a, Show b, Eq a, Eq b, Function a, Function b)
      => Simple Iso a b -> Property
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
