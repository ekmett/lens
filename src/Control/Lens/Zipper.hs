{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Lens.Zipper
  (
  -- * Zippers
    Top()
  , (:>)()
  -- ** Focusing
  , focus
  -- ** Moving
  , left
  , lefts
  , right
  , rights
  , up
  , down
  , amid
  -- ** Closing the Zipper
  , close
  , Zipped
  , Zipper()
  -- ** Saving your Progress
  , Tape()
  , save
  , restore
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad ((>=>))
import Control.Lens
import Control.Lens.Internal
import Data.List.NonEmpty as NonEmpty

data Top
data p :> a = Zipper (Coil p a) {-# UNPACK #-} !(Level a)

type family Zipped h a
type instance Zipped Top a      = a
type instance Zipped (h :> b) a = Zipped h b

data Coil :: * -> * -> * where
  Coil :: Coil Top a
  Snoc :: Coil h b ->
          {-# UNPACK #-} !Int -> SimpleLensLike (Bazaar a a) b a ->
          [b] -> (NonEmpty a -> b) -> [b] ->
          Coil (h :> b) a

level :: Simple Lens (h :> a) (Level a)
level f (Zipper h a) = Zipper h <$> f a
{-# INLINE level #-}

focus :: Simple Lens (h :> a) a
focus = level.focusLevel
{-# INLINE focus #-}

zipper :: a -> Top :> a
zipper a = Zipper Coil (Level 0 [] a [])
{-# INLINE zipper #-}

up :: (a :> b :> c) -> a :> b
up (Zipper (Snoc h n _ ls k rs) w) = Zipper h (Level n ls (k (closeLevel w)) rs)
{-# INLINE up #-}

left  :: (a :> b) -> Maybe (a :> b)
left (Zipper h w) = Zipper h <$> leftLevel w
{-# INLINE left #-}

right :: (a :> b) -> Maybe (a :> b)
right (Zipper h w) = Zipper h <$> rightLevel w
{-# INLINE right #-}

down :: SimpleLensLike (Context c c) b c -> (a :> b) -> a :> b :> c
down l (Zipper h (Level n ls b rs)) = case l (Context id) b of
  Context k c -> Zipper (Snoc h n (cloneLens l) ls (k . extract) rs) (Level 0 [] c [])
{-# INLINE down #-}

amid :: SimpleLensLike (Bazaar c c) b c -> (a :> b) -> Maybe (a :> b :> c)
amid l (Zipper h (Level n ls b rs)) = case partsOf l (Context id) b of
  Context _ []     -> Nothing
  Context k (c:cs) -> Just (Zipper (Snoc h n l ls (k . NonEmpty.toList) rs) (Level 0 [] c cs))
{-# INLINE amid #-}

class Zipper h a where
  recoil :: Coil h a -> NonEmpty a -> Zipped h a

instance Zipper Top a where
  recoil Coil = extract

instance Zipper h b => Zipper (h :> b) c where
  recoil (Snoc h _ _ ls k rs) as = recoil h (NonEmpty.fromList (Prelude.reverse ls ++ k as : rs))

close :: Zipper h a => (h :> a) -> Zipped h a
close (Zipper h w) = recoil h (closeLevel w)
{-# INLINE close #-}

peel :: Coil h a -> Track h a
peel Coil               = Track
peel (Snoc h n l _ _ _) = Fork (peel h) n l

data Track :: * -> * -> * where
  Track :: Track Top a
  Fork  :: Track h b -> {-# UNPACK #-} !Int -> SimpleLensLike (Bazaar a a) b a -> Track (h :> b) a

restoreTrack :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreTrack Track = Just . zipper
restoreTrack (Fork h n l) = restoreTrack h >=> rights n >=> amid l where

rights :: Int -> (h :> a) -> Maybe (h :> a)
rights 0 z = Just z
rights k z = right z >>= rights (k - 1)

lefts :: Int -> (h :> a) -> Maybe (h :> a)
lefts 0 z = Just z
lefts k z = left z >>= lefts (k - 1)

data Tape k where
  Tape :: Track h a -> {-# UNPACK #-} !Int -> Tape (h :> a)

save :: (a :> b) -> Tape (a :> b)
save (Zipper h (Level n _ _ _)) = Tape (peel h) n
{-# INLINE save #-}

restore :: Tape (h :> a) -> Zipped h a -> Maybe (h :> a)
restore (Tape h n) = restoreTrack h >=> rights n
{-# INLINE restore #-}
