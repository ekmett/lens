{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A deliberately ill-typed fixture: it MUST NOT compile.
--
-- It is the negative half of the test suite — proof that 'build' rejects an
-- incomplete record at compile time, with our custom message
-- @build: missing field \"v\"@. The @build-negative-test@ Cabal flag gates this
-- executable so it is never part of a normal build; @tests/negative/run.sh@
-- turns the flag on and asserts that compiling it fails with that message.
--
-- Do not "fix" the missing field below — failing to compile is the point.
module Main (main) where

import Control.Lens ((&))
import Control.Lens.Build (record, field, build)
import Control.Lens.Build.TH (makeBuild)

data P = P { _u :: Int, _v :: Int }
makeBuild ''P

-- | Omits field @\"v\"@. @Complete P '["u"]@ reduces via @Elem "v" '["u"] = 'False@
-- to our @TypeError@, so GHC rejects this binding.
oops :: P
oops = record & field @"u" 1 & build

main :: IO ()
main = oops `seq` pure ()
