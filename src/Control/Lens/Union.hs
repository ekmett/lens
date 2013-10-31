{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Tuple
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-------------------------------------------------------------------------------
module Control.Lens.Union
       ( VariantA (..)
       , VariantB (..)
       , VariantC (..)
       , VariantD (..)
       , VariantE (..)
       , VariantF (..)
       , VariantG (..)
       , VariantH (..)
       , VariantI (..)
       ) where

import Control.Applicative
import Control.Lens.Internal.Nat
import Control.Lens.Prism
import Data.Profunctor
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..), (:+:) (..), (:*:) (..), K1 (..), M1 (..), U1 (..))

-- | Provides access to 1st variant of a tagged union.
class VariantA s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 1st variant of a tagged union (and possibly change its type).
  _A :: Prism s t a b
#ifndef HLINT
  default _A :: (Generic s, Generic t, GIxed N0 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _A #-}
  _A = ix (Proxy :: Proxy N0)
#endif

-- | Provides access to the 2nd variant of a tagged union.
class VariantB s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 2nd variant of a tagged union.
  _B :: Prism s t a b
#ifndef HLINT
  default _B :: (Generic s, Generic t, GIxed N1 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _B #-}
  _B = ix (Proxy :: Proxy N1)
#endif

-- | Provides access to the 3rd variant of a tagged union.
class VariantC s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 3rd variant of a tagged union.
  _C :: Prism s t a b
#ifndef HLINT
  default _C :: (Generic s, Generic t, GIxed N2 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _C #-}
  _C = ix (Proxy :: Proxy N2)
#endif

-- | Provides access to the 4th variant of a tagged union.
class VariantD s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 4th variant of a tagged union.
  _D :: Prism s t a b
#ifndef HLINT
  default _D :: (Generic s, Generic t, GIxed N3 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _D #-}
  _D = ix (Proxy :: Proxy N3)
#endif

-- | Provides access to the 5th variant of a tagged union.
class VariantE s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 5th variant of a tagged union.
  _E :: Prism s t a b
#ifndef HLINT
  default _E :: (Generic s, Generic t, GIxed N4 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _E #-}
  _E = ix (Proxy :: Proxy N4)
#endif

-- | Provides access to the 6th variant of a tagged union.
class VariantF s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 6th variant of a tagged union.
  _F :: Prism s t a b
#ifndef HLINT
  default _F :: (Generic s, Generic t, GIxed N5 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _F #-}
  _F = ix (Proxy :: Proxy N5)
#endif

-- | Provides access to the 7th variant of a tagged union.
class VariantG s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 7th variant of a tagged union.
  _G :: Prism s t a b
#ifndef HLINT
  default _G :: (Generic s, Generic t, GIxed N6 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _G #-}
  _G = ix (Proxy :: Proxy N6)
#endif

-- | Provides access to the 8th variant of a tagged union.
class VariantH s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 8th variant of a tagged union.
  _H :: Prism s t a b
#ifndef HLINT
  default _H :: (Generic s, Generic t, GIxed N7 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _H #-}
  _H = ix (Proxy :: Proxy N7)
#endif

-- | Provides access to the 9th variant of a tagged union.
class VariantI s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 9th variant of a tagged union.
  _I :: Prism s t a b
#ifndef HLINT
  default _I :: (Generic s, Generic t, GIxed N8 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _I #-}
  _I = ix (Proxy :: Proxy N8)
#endif

instance VariantA Bool Bool () ()
instance VariantB Bool Bool () ()
instance VariantA Ordering Ordering () ()
instance VariantB Ordering Ordering () ()
instance VariantC Ordering Ordering () ()
instance VariantA (Maybe a) (Maybe a) () ()
instance VariantB (Maybe a) (Maybe b) a b
instance VariantA (Either a c) (Either b c) a b
instance VariantB (Either c a) (Either c b) a b
instance VariantA [a] [a] () ()
instance VariantB [a] [b] (a, [a]) (b, [b])

ix :: (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b)
   => f n -> Prism s t a b
{-# INLINE ix #-}
ix n = dimap from (fmap to) . gix n

#ifndef HLINT
class GIxed n s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gix :: f n -> Prism (s x) (t x) a b
#endif

instance GIxed N0 U1 U1 () () where
  {-# INLINE gix #-}
  gix _ = prism (const U1) (const $ Right ())

instance GIxed N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gix #-}
  gix _ = prism K1 (Right . unK1)

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gix #-}
  gix n = dimap unM1 (fmap M1) . gix n

instance GIxed' (GT (GSize s) n) n s s' t t' a b
      => GIxed n (s :+: s') (t :+: t') a b where
  {-# INLINE gix #-}
  gix n = gix' (reproxySizeGT (Proxy :: Proxy (s x)) n) n

instance (IsGTuple s, IsGTuple s', IsGTuple t, IsGTuple t',
          IsTuple (GList (s :*: s')), IsTuple (GList (t :*: t')),
          a ~ ToTuple (s :*: s'), b ~ ToTuple (t :*: t'))
      => GIxed N0 (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix _ = dimap (toTuple . toGTuple) (fmap $ fromGTuple . fromTuple)

#ifndef HLINT
class GIxed' p n s s' t t' a b where
  gix' :: f p -> g n -> Prism ((s :+: s') x) ((t :+: t') x) a b
#endif

instance (GIxed n s t a b, s' ~ t') => GIxed' True n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = dimap (gsum Left Right) (either (fmap L1) (pure . R1)) . left' . gix n

instance (GIxed (Subtract (GSize s) n) s' t' a b, s ~ t)
      => GIxed' False n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = dimap (gsum Left Right) (either (pure . L1) (fmap R1)) . right' .
    gix (reproxySubtractSize (Proxy :: Proxy (s x)) n)

#ifndef HLINT
data GTuple xs where
  U :: GTuple '[]
  (:*) :: x -> !(GTuple xs) -> GTuple (x ': xs)
#endif

infixr 5 :*

#ifndef HLINT
uncons :: (a -> GTuple as -> r) -> GTuple (a ': as) -> r
{-# INLINE uncons #-}
uncons f (a :* as) = f a as
#endif

#ifndef HLINT
unnil :: r -> GTuple '[] -> r
{-# INLINE unnil #-}
unnil r U = r
#endif

class IsTuple xs where
  type Tuple xs
  toTuple :: GTuple xs -> Tuple xs
  fromTuple :: Tuple xs -> GTuple xs

#ifndef HLINT
instance IsTuple '[] where
  type Tuple '[] = ()
  {-# INLINE toTuple #-}
  toTuple _ = ()
  {-# INLINE fromTuple #-}
  fromTuple _ = U
#endif

#ifndef HLINT
instance IsTuple '[a] where
  type Tuple '[a] = a
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    unnil a
  {-# INLINE fromTuple #-}
  fromTuple a = a :* U
#endif

#ifndef HLINT
instance IsTuple [a, b] where
  type Tuple [a, b] = (a, b)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    unnil (a, b)
  {-# INLINE fromTuple #-}
  fromTuple (a, b) = a :* b :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c] where
  type Tuple [a, b, c] = (a, b, c)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    unnil (a, b, c)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c) = a :* b :* c :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d] where
  type Tuple [a, b, c, d] = (a, b, c, d)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    unnil (a, b, c, d)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d) = a :* b :* c :* d :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e] where
  type Tuple [a, b, c, d, e] = (a, b, c, d, e)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    unnil (a, b, c, d, e)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e) = a :* b :* c :* d :* e :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f] where
  type Tuple [a, b, c, d, e, f] = (a, b, c, d, e, f)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    unnil (a, b, c, d, e, f)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f) = a :* b :* c :* d :* e :* f :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g] where
  type Tuple [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    unnil (a, b, c, d, e, f, g)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g) = a :* b :* c :* d :* e :* f :* g :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g, h] where
  type Tuple [a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    uncons $ \ h ->
    unnil (a, b, c, d, e, f, g, h)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g, h) = a :* b :* c :* d :* e :* f :* g :* h :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g, h, i] where
  type Tuple [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    uncons $ \ h ->
    uncons $ \ i ->
    unnil (a, b, c, d, e, f, g, h, i)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g, h, i) = a :* b :* c :* d :* e :* f :* g :* h :* i :* U
#endif

type ToTuple s = Tuple (GList s)

class IsGTuple s where
#ifndef HLINT
  type GCons s xs :: [*]
#endif
  gcons :: s x -> GTuple xs -> GTuple (GCons s xs)
  guncons :: (s x -> GTuple xs -> r) -> GTuple (GCons s xs) -> r

#ifndef HLINT
type GList s = GCons s '[]
#endif

toGTuple :: IsGTuple s => s x -> GTuple (GList s)
{-# INLINE toGTuple #-}
toGTuple = flip gcons U

fromGTuple :: IsGTuple s => GTuple (GList s) -> s x
{-# INLINE fromGTuple #-}
fromGTuple = guncons unnil

instance IsGTuple U1 where
  type GCons U1 xs = xs
  {-# INLINE gcons #-}
  gcons = flip const
  {-# INLINE guncons #-}
  guncons = ($ U1)

instance IsGTuple (K1 i c) where
#ifndef HLINT
  type GCons (K1 i c) xs = c ': xs
#endif
  {-# INLINE gcons #-}
  gcons = (:*) . unK1
  {-# INLINE guncons #-}
  guncons f = uncons $ f . K1

instance IsGTuple f => IsGTuple (M1 i c f) where
  type GCons (M1 i c f) xs = GCons f xs
  {-# INLINE gcons #-}
  gcons = gcons . unM1
  {-# INLINE guncons #-}
  guncons f = guncons $ f . M1

instance (IsGTuple a, IsGTuple b) => IsGTuple (a :*: b) where
  type GCons (a :*: b) xs = GCons a (GCons b xs)
  {-# INLINE gcons #-}
  gcons (a :*: b) = gcons a . gcons b
  {-# INLINE guncons #-}
  guncons f = guncons $ \ a -> guncons $ \ b -> f $ a :*: b

gsum :: (a x -> r) -> (b x -> r) -> (a :+: b) x -> r
{-# INLINE gsum #-}
gsum f _ (L1 a) = f a
gsum _ f (R1 a) = f a

type family GSize (f :: * -> *)
type instance GSize U1 = S Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :+: b) = Add (GSize a) (GSize b)
type instance GSize (a :*: b) = S Z

reproxySubtractSize :: f (s x) -> g n -> Proxy (Subtract (GSize s) n)
{-# INLINE reproxySubtractSize #-}
reproxySubtractSize _ _ = Proxy

reproxySizeGT :: f (s x) -> g n -> Proxy (GT (GSize s) n)
{-# INLINE reproxySizeGT #-}
reproxySizeGT _ _ = Proxy
