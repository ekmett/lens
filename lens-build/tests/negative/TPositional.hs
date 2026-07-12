{-# LANGUAGE TemplateHaskell #-}

-- | A deliberately rejected fixture: it MUST NOT compile.
--
-- 'makeBuild' needs /named/ fields, so a single positional constructor must be
-- rejected at splice time with
-- @makeBuild: a single record constructor with named fields is required@.
-- Gated behind the @build-negative-test@ flag; see @tests/negative/run.sh@.
module Main (main) where

import Control.Lens.Build.TH (makeBuild)

data N = N Int Int

makeBuild ''N

main :: IO ()
main = pure ()
