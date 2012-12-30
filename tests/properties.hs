{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Lens
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Data.Char (isAlphaNum, isAscii, toUpper)
import Data.Text.Strict.Lens
import Data.List.Lens
import Data.Functor.Compose
import Numeric (showHex, showOct, showSigned)
import Numeric.Lens


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

iso_hither :: Eq s => Simple AnIso s a -> s -> Bool
iso_hither l s = s ^.cloneIso l.from l == s

iso_yon :: Eq a => Simple AnIso s a -> a -> Bool
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

isSetter :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Setter' s a -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l

isTraversal :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Traversal' s a -> Property
isTraversal l = isSetter l .&. traverse_pureMaybe l .&. traverse_pureList l
                  .&. do as <- arbitrary
                         bs <- arbitrary
                         t <- arbitrary
                         property $ traverse_compose l (\x -> as++[x]++bs)
                                                       (\x -> if t then Just x else Nothing)

isLens :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
       => Lens' s a -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
      => Iso' s a -> Property
isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)

isPrism :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
      => Prism' s a -> Property
isPrism l = isTraversal l .&. prism_yin l .&. prism_yang l

-- an illegal lens
bad :: Lens' (Int,Int) Int
bad f (a,b) = (,) b <$> f a

badIso :: Iso' Int Bool
badIso = iso even fromEnum

-- Control.Lens.Type
prop_1                               = isLens (_1 :: Lens' (Int,Double,()) Int)
prop_2                               = isLens (_2 :: Lens' (Int,Bool) Bool)
prop_3                               = isLens (_3 :: Lens' (Int,Bool,()) ())
prop_4                               = isLens (_4 :: Lens' (Int,Bool,(),Maybe Int) (Maybe Int))
prop_5                               = isLens (_5 :: Lens' ((),(),(),(),Int) Int)

prop_2_2                             = isLens (_2._2 :: Lens' (Int,(Int,Bool),Double) Bool)

-- prop_illegal_lens                    = expectFailure $ isLens bad
-- prop_illegal_traversal               = expectFailure $ isTraversal bad
-- prop_illegal_setter                  = expectFailure $ isSetter bad
-- prop_illegal_iso                     = expectFailure $ isIso badIso

-- Control.Lens.Setter
prop_mapped                          = isSetter (mapped :: Setter' [Int] Int)
prop_mapped_mapped                   = isSetter (mapped.mapped :: Setter' [Maybe Int] Int)

prop_both                            = isTraversal (both      :: Traversal' (Int,Int) Int)
prop_value (Fun _ k :: Fun Int Bool) = isTraversal (traversed.indices k :: Traversal' [Int] Int)
prop_traverseLeft                    = isTraversal (_left     :: Traversal' (Either Int Bool) Int)
prop_traverseRight                   = isTraversal (_right    :: Traversal' (Either Int Bool) Bool)

prop_simple                          = isIso (simple :: Iso' Int Int)
--prop_enum                            = isIso (enum :: Iso' Int Char)

prop__left                           = isPrism (_left :: Prism' (Either Int Bool) Int)
prop__right                          = isPrism (_right :: Prism' (Either Int Bool) Bool)
prop__just                           = isPrism (_just :: Prism' (Maybe Int) Int)

-- Data.List.Lens
prop_strippingPrefix s               = isPrism (strippingPrefix s :: Prism' String String)

-- Data.Text.Lens
prop_text s                          = s^.packed.from packed == s
--prop_text                           = isIso packed

-- Numeric.Lens
prop_base_show (n :: Integer) =
  conjoin [ show n == n ^. remit (base 10)
          , showSigned showOct 0 n "" == n ^. remit (base 8)
          , showSigned showHex 0 n "" == n ^. remit (base 16)
          ]
prop_base_read (n :: Integer) =
  conjoin [ show n ^? base 10 == Just n
          , showSigned showOct 0 n "" ^? base 8  == Just n
          , showSigned showHex 0 n "" ^? base 16 == Just n
          , map toUpper (showSigned showHex 0 n "") ^? base 16 == Just n
          ]
prop_base_readFail (s :: String) =
  forAll (choose (2,36)) $ \b ->
    not isValid ==> s ^? base b == Nothing
  where
    isValid = (not . null) sPos && all isValidChar sPos
    sPos = case s of { ('-':s') -> s'; _ -> s }
    isValidChar c = isAscii c && isAlphaNum c

main :: IO ()
main = $defaultMainGenerator
