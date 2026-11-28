{-# LANGUAGE TemplateHaskell #-}

-- | A deliberately rejected fixture: it MUST NOT compile.
--
-- 'makeBuild' supports monomorphic records only, so a type with a parameter
-- must be rejected at splice time with
-- @makeBuild: only monomorphic records (no type parameters) are supported@.
-- Gated behind the @build-negative-test@ flag; see @tests/negative/run.sh@.
module Main (main) where

import Control.Lens.Build.TH (makeBuild)

data Q a = Q { _w :: a }

makeBuild ''Q

main :: IO ()
main = pure ()
