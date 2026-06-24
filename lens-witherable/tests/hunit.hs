module Main (main) where

import Control.Lens
import Control.Lens.PartialIso
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Read (readMaybe)

-- A PartialIso between String and Int via readMaybe/show.
intPI :: PartialIso' String Int
intPI = partialIso readMaybe (Just . show)

case_partialiso_get_hit :: IO ()
case_partialiso_get_hit =
  "42" ^? getting intPI @?= Just (42 :: Int)

case_partialiso_get_miss :: IO ()
case_partialiso_get_miss =
  ("frob" ^? getting intPI :: Maybe Int) @?= Nothing

case_partialiso_review_hit :: IO ()
case_partialiso_review_hit =
  intPI #? (5 :: Int) @?= Just "5"

case_invprism_get :: IO ()
case_invprism_get =
  ((3 :: Int) ^? getting (invPrism _Left) :: Maybe (Either Int String))
    @?= Just (Left 3)

case_failing'_partialiso_wins :: IO ()
case_failing'_partialiso_wins =
  "100" ^? getting (failing' intPI (_Show :: Prism' String Int))
    @?= Just (100 :: Int)

case_failing'_prism_fallback :: IO ()
case_failing'_prism_fallback =
  failing' intPI (_Show :: Prism' String Int) # (7 :: Int) @?= "7"

main :: IO ()
main = defaultMain $
  testGroup "lens-witherable"
  [ testCase "partialIso get hit" case_partialiso_get_hit
  , testCase "partialIso get miss" case_partialiso_get_miss
  , testCase "partialIso review hit" case_partialiso_review_hit
  , testCase "invPrism get" case_invprism_get
  , testCase "failing' partialIso wins" case_failing'_partialiso_wins
  , testCase "failing' prism fallback" case_failing'_prism_fallback
  ]
