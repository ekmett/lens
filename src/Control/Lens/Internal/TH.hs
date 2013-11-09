{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.TH
-- Copyright   :  (C) 2013 Edward Kmett, 2013 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.TH where

import Language.Haskell.TH

-- | Compatibility shim for recent changes to template haskell's 'tySynInstD'
tySynInstD' :: Name -> [TypeQ] -> TypeQ -> DecQ
#if MIN_VERSION_template_haskell(2,9,0)
tySynInstD' fam ts r = tySynInstD fam (tySynEqn ts r)
#else
tySynInstD' fam ts r = tySynInstD fam ts r
#endif

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

appsE1 :: ExpQ -> [ExpQ] -> ExpQ
appsE1 = foldl appE

toTupleT :: [TypeQ] -> TypeQ
toTupleT [x] = x
toTupleT xs = appsT (tupleT (length xs)) xs

toTupleE :: [ExpQ] -> ExpQ
toTupleE [x] = x
toTupleE xs = tupE xs

toTupleP :: [PatQ] -> PatQ
toTupleP [x] = x
toTupleP xs = tupP xs

