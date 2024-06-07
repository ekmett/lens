{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Test 'makeFieldsId', which requires NoFieldSelectors and
-- DuplicateRecordFields. This test consequently only works on GHC >= 9.2.
module T1024 where

import Control.Lens

data Taco = Taco
  { hardShell :: Bool
  , sauce :: Int
  , filling :: String
  }
data Burrito = Burrito
  { sauce :: Int
  , filling :: String
  }
makeFieldsId ''Taco
makeFieldsId ''Burrito

checkTacoHardShell :: Lens' Taco Bool
checkTacoHardShell = hardShell

checkBurritoFilling :: Lens' Burrito String
checkBurritoFilling = filling
