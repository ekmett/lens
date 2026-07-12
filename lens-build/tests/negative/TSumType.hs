{-# LANGUAGE TemplateHaskell #-}

-- | A deliberately rejected fixture: it MUST NOT compile.
--
-- 'makeBuild' supports single-constructor records only, so a sum type must be
-- rejected at splice time with @makeBuild: single-constructor records only@.
-- Gated behind the @build-negative-test@ flag; see @tests/negative/run.sh@.
module Main (main) where

import Control.Lens.Build.TH (makeBuild)

data S = A | B

makeBuild ''S

main :: IO ()
main = pure ()
