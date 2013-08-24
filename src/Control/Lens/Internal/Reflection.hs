{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
----------------------------------------------------------------------------
-- |
-- Module     : Control.Lens.Internal.Reflection
-- Copyright  : 2009-2013 Edward Kmett,
--              2012 Elliott Hird,
--              2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Reifies arbitrary terms at the type level. Based on the Functional
-- Pearl: Implicit Configurations paper by Oleg Kiselyov and
-- Chung-chieh Shan.
--
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>
--
-- The approach from the paper was modified to work with Data.Proxy
-- and streamline the API by Edward Kmett and Elliott Hird.
--
-- Usage comes down to two combinators, 'reify' and 'reflect'.
--
-- >>> reify 6 (\p -> reflect p + reflect p)
-- 12
--
-- The argument passed along by reify is just a @data 'Proxy' t =
-- Proxy@, so all of the information needed to reconstruct your value
-- has been moved to the type level.  This enables it to be used when
-- constructing instances (see @examples/Monoid.hs@).
--
-- This version is based on the \"slow\" path from the @reflection@ package,
-- but modified to work with the same 'Reifies' class as is provided by the \"fast\"
-- path, and to make sure the parameter is 'Typeable'.
--
-- This is necessary to work around the changes to @Data.Typeable@ in GHC HEAD.
-------------------------------------------------------------------------------

module Control.Lens.Internal.Reflection
    (
      Reifies(..)
    , reifyTypeable
    ) where

import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe
import Control.Applicative
import Data.Proxy
import Data.Bits
import Data.Word
import Data.Typeable
import Data.Reflection

{-# ANN module "HLint: ignore Avoid lambda" #-}

class Typeable s => B s where
  reflectByte :: proxy s -> IntPtr

#define CAT(a,b) a/**/b

#define BYTES(GO) \
  GO(0) GO(1) GO(2) GO(3) GO(4) GO(5) GO(6) GO(7) GO(8) GO(9) GO(10) GO(11) \
  GO(12) GO(13) GO(14) GO(15) GO(16) GO(17) GO(18) GO(19) GO(20) GO(21) GO(22) \
  GO(23) GO(24) GO(25) GO(26) GO(27) GO(28) GO(29) GO(30) GO(31) GO(32) GO(33) \
  GO(34) GO(35) GO(36) GO(37) GO(38) GO(39) GO(40) GO(41) GO(42) GO(43) GO(44) \
  GO(45) GO(46) GO(47) GO(48) GO(49) GO(50) GO(51) GO(52) GO(53) GO(54) GO(55) \
  GO(56) GO(57) GO(58) GO(59) GO(60) GO(61) GO(62) GO(63) GO(64) GO(65) GO(66) \
  GO(67) GO(68) GO(69) GO(70) GO(71) GO(72) GO(73) GO(74) GO(75) GO(76) GO(77) \
  GO(78) GO(79) GO(80) GO(81) GO(82) GO(83) GO(84) GO(85) GO(86) GO(87) GO(88) \
  GO(89) GO(90) GO(91) GO(92) GO(93) GO(94) GO(95) GO(96) GO(97) GO(98) GO(99) \
  GO(100) GO(101) GO(102) GO(103) GO(104) GO(105) GO(106) GO(107) GO(108) \
  GO(109) GO(110) GO(111) GO(112) GO(113) GO(114) GO(115) GO(116) GO(117) \
  GO(118) GO(119) GO(120) GO(121) GO(122) GO(123) GO(124) GO(125) GO(126) \
  GO(127) GO(128) GO(129) GO(130) GO(131) GO(132) GO(133) GO(134) GO(135) \
  GO(136) GO(137) GO(138) GO(139) GO(140) GO(141) GO(142) GO(143) GO(144) \
  GO(145) GO(146) GO(147) GO(148) GO(149) GO(150) GO(151) GO(152) GO(153) \
  GO(154) GO(155) GO(156) GO(157) GO(158) GO(159) GO(160) GO(161) GO(162) \
  GO(163) GO(164) GO(165) GO(166) GO(167) GO(168) GO(169) GO(170) GO(171) \
  GO(172) GO(173) GO(174) GO(175) GO(176) GO(177) GO(178) GO(179) GO(180) \
  GO(181) GO(182) GO(183) GO(184) GO(185) GO(186) GO(187) GO(188) GO(189) \
  GO(190) GO(191) GO(192) GO(193) GO(194) GO(195) GO(196) GO(197) GO(198) \
  GO(199) GO(200) GO(201) GO(202) GO(203) GO(204) GO(205) GO(206) GO(207) \
  GO(208) GO(209) GO(210) GO(211) GO(212) GO(213) GO(214) GO(215) GO(216) \
  GO(217) GO(218) GO(219) GO(220) GO(221) GO(222) GO(223) GO(224) GO(225) \
  GO(226) GO(227) GO(228) GO(229) GO(230) GO(231) GO(232) GO(233) GO(234) \
  GO(235) GO(236) GO(237) GO(238) GO(239) GO(240) GO(241) GO(242) GO(243) \
  GO(244) GO(245) GO(246) GO(247) GO(248) GO(249) GO(250) GO(251) GO(252) \
  GO(253) GO(254) GO(255)

#define GO(n) \
  newtype CAT(T,n) = CAT(T,n) CAT(T,n) deriving Typeable; \
  instance B CAT(T,n) where { \
    reflectByte _ = n \
  };
BYTES(GO)
#undef GO

impossible :: a
impossible = error "Data.Reflection.reifyByte: impossible"

reifyByte :: Word8 -> (forall s. B s => Proxy s -> r) -> r
reifyByte w k = case w of {
#define GO(n) n -> k (Proxy :: Proxy CAT(T,n));
BYTES(GO)
#undef GO
_ -> impossible
}

newtype W b0 b1 b2 b3 = W (W b0 b1 b2 b3) deriving Typeable
newtype Stable w0 w1 a = Stable (Stable w0 w1 a) deriving Typeable

stable :: p b0 -> p b1 -> p b2 -> p b3 -> p b4 -> p b5 -> p b6 -> p b7
       -> Proxy (Stable (W b0 b1 b2 b3) (W b4 b5 b6 b7) a)
stable _ _ _ _ _ _ _ _ = Proxy
{-# INLINE stable #-}

stablePtrToIntPtr :: StablePtr a -> IntPtr
stablePtrToIntPtr = ptrToIntPtr . castStablePtrToPtr
{-# INLINE stablePtrToIntPtr #-}

intPtrToStablePtr :: IntPtr -> StablePtr a
intPtrToStablePtr = castPtrToStablePtr . intPtrToPtr
{-# INLINE intPtrToStablePtr #-}

byte0 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b0
byte0 _ = Proxy

byte1 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b1
byte1 _ = Proxy

byte2 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b2
byte2 _ = Proxy

byte3 :: p (Stable (W b0 b1 b2 b3) w1 a) -> Proxy b3
byte3 _ = Proxy

byte4 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b4
byte4 _ = Proxy

byte5 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b5
byte5 _ = Proxy

byte6 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b6
byte6 _ = Proxy

byte7 :: p (Stable w0 (W b4 b5 b6 b7) a) -> Proxy b7
byte7 _ = Proxy

argument :: (p s -> r) -> Proxy s
argument _ = Proxy

instance (B b0, B b1, B b2, B b3, B b4, B b5, B b6, B b7, w0 ~ W b0 b1 b2 b3, w1 ~ W b4 b5 b6 b7)
    => Reifies (Stable w0 w1 a) a where
  reflect = r where
      r = unsafePerformIO $ const <$> deRefStablePtr p <* freeStablePtr p
      s = argument r
      p = intPtrToStablePtr $
        reflectByte (byte0 s) .|.
        (reflectByte (byte1 s) `shiftL` 8) .|.
        (reflectByte (byte2 s) `shiftL` 16) .|.
        (reflectByte (byte3 s) `shiftL` 24) .|.
        (reflectByte (byte4 s) `shiftL` 32) .|.
        (reflectByte (byte5 s) `shiftL` 40) .|.
        (reflectByte (byte6 s) `shiftL` 48) .|.
        (reflectByte (byte7 s) `shiftL` 56)
  {-# NOINLINE reflect #-}

-- This had to be moved to the top level, due to an apparent bug in
-- the ghc inliner introduced in ghc 7.0.x
reflectBefore :: Reifies s a => (Proxy s -> b) -> proxy s -> b
reflectBefore f = const $! f Proxy
{-# NOINLINE reflectBefore #-}

-- | Reify a value at the type level in a 'Typeable'-compatible fashion, to be recovered with 'reflect'.
reifyTypeable :: Typeable a => a -> (forall s. (Typeable s, Reifies s a) => Proxy s -> r) -> r
reifyTypeable a k = unsafeDupablePerformIO $ do
  p <- newStablePtr a
  let n = stablePtrToIntPtr p
  reifyByte (fromIntegral n) (\s0 ->
    reifyByte (fromIntegral (n `shiftR` 8)) (\s1 ->
      reifyByte (fromIntegral (n `shiftR` 16)) (\s2 ->
        reifyByte (fromIntegral (n `shiftR` 24)) (\s3 ->
          reifyByte (fromIntegral (n `shiftR` 32)) (\s4 ->
            reifyByte (fromIntegral (n `shiftR` 40)) (\s5 ->
              reifyByte (fromIntegral (n `shiftR` 48)) (\s6 ->
                reifyByte (fromIntegral (n `shiftR` 56)) (\s7 ->
                  reflectBefore (fmap return k) $
                    stable s0 s1 s2 s3 s4 s5 s6 s7))))))))
