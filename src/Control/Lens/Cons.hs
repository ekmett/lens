{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Cons
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Cons
  (
  -- * Cons
    Cons(..)
  , (<|)
  , cons
  , uncons
  , _head, _tail
  , (<|~), (<|=), (<<|~), (<<|=)
  , pattern (:<)
  -- * Snoc
  , Snoc(..)
  , (|>)
  , snoc
  , unsnoc
  , _init, _last
  , (|>~), (|>=), (<|>~), (<|>=)
  , pattern (:>)

  ) where

import Control.Lens.Equality (simply)
import Control.Lens.Fold
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Type
import qualified Data.ByteString      as StrictB
import qualified Data.ByteString.Lazy as LazyB
import           Data.Coerce
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, ViewL(EmptyL), ViewR(EmptyR), viewl, viewr)
import qualified Data.Text      as StrictT
import qualified Data.Text.Lazy as LazyT
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as Storable
import           Data.Vector.Primitive (Prim)
import qualified Data.Vector.Primitive as Prim
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unbox
import           Data.Word
import           Control.Applicative (ZipList(..))
import           Control.Monad.State.Class as State
import           Prelude

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import qualified Data.Sequence as Seq
-- >>> import qualified Data.Vector as Vector
-- >>> import qualified Data.Text.Lazy as LazyT
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

infixr 5 <|, `cons`
infixl 5 |>, `snoc`
infixr 4 <|~, |>~, <<|~, <|>~
infix  4 <|=, |>=, <<|=, <|>=

pattern (:<) :: Cons b b a a => a -> b -> b
pattern (:<) a s <- (preview _Cons -> Just (a,s)) where
  (:<) a s = _Cons # (a,s)

infixr 5 :<
infixl 5 :>

pattern (:>) :: Snoc a a b b => a -> b -> a
pattern (:>) s a <- (preview _Snoc -> Just (s,a)) where
  (:>) a s = _Snoc # (a,s)

------------------------------------------------------------------------------
-- Cons
------------------------------------------------------------------------------

-- | This class provides a way to attach or detach elements on the left
-- side of a structure in a flexible manner.
class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- |
  --
  -- @
  -- '_Cons' :: 'Prism' [a] [b] (a, [a]) (b, [b])
  -- '_Cons' :: 'Prism' ('Seq' a) ('Seq' b) (a, 'Seq' a) (b, 'Seq' b)
  -- '_Cons' :: 'Prism' ('Vector' a) ('Vector' b) (a, 'Vector' a) (b, 'Vector' b)
  -- '_Cons' :: 'Prism'' 'String' ('Char', 'String')
  -- '_Cons' :: 'Prism'' 'StrictT.Text' ('Char', 'StrictT.Text')
  -- '_Cons' :: 'Prism'' 'StrictB.ByteString' ('Word8', 'StrictB.ByteString')
  -- @
  _Cons :: Prism s t (a,s) (b,t)

instance Cons [a] [b] a b where
  _Cons = prism (uncurry (:)) $ \ aas -> case aas of
    (a:as) -> Right (a, as)
    []     -> Left  []
  {-# INLINE _Cons #-}

instance Cons (ZipList a) (ZipList b) a b where
  _Cons = withPrism listCons $ \listReview listPreview ->
    prism (coerce listReview) (coerce listPreview) where

    listCons :: Prism [a] [b] (a, [a]) (b, [b])
    listCons = _Cons

  {-# INLINE _Cons #-}

instance Cons (Seq a) (Seq b) a b where
  _Cons = prism (uncurry (Seq.<|)) $ \aas -> case viewl aas of
    a Seq.:< as -> Right (a, as)
    EmptyL  -> Left mempty
  {-# INLINE _Cons #-}

instance Cons StrictB.ByteString StrictB.ByteString Word8 Word8 where
  _Cons = prism' (uncurry StrictB.cons) StrictB.uncons
  {-# INLINE _Cons #-}

instance Cons LazyB.ByteString LazyB.ByteString Word8 Word8 where
  _Cons = prism' (uncurry LazyB.cons) LazyB.uncons
  {-# INLINE _Cons #-}

instance Cons StrictT.Text StrictT.Text Char Char where
  _Cons = prism' (uncurry StrictT.cons) StrictT.uncons
  {-# INLINE _Cons #-}

instance Cons LazyT.Text LazyT.Text Char Char where
  _Cons = prism' (uncurry LazyT.cons) LazyT.uncons
  {-# INLINE _Cons #-}

instance Cons (Vector a) (Vector b) a b where
  _Cons = prism (uncurry Vector.cons) $ \v ->
    if Vector.null v
    then Left Vector.empty
    else Right (Vector.unsafeHead v, Vector.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Prim a, Prim b) => Cons (Prim.Vector a) (Prim.Vector b) a b where
  _Cons = prism (uncurry Prim.cons) $ \v ->
    if Prim.null v
    then Left Prim.empty
    else Right (Prim.unsafeHead v, Prim.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Storable a, Storable b) => Cons (Storable.Vector a) (Storable.Vector b) a b where
  _Cons = prism (uncurry Storable.cons) $ \v ->
    if Storable.null v
    then Left Storable.empty
    else Right (Storable.unsafeHead v, Storable.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Unbox a, Unbox b) => Cons (Unbox.Vector a) (Unbox.Vector b) a b where
  _Cons = prism (uncurry Unbox.cons) $ \v ->
    if Unbox.null v
    then Left Unbox.empty
    else Right (Unbox.unsafeHead v, Unbox.unsafeTail v)
  {-# INLINE _Cons #-}

-- | 'cons' an element onto a container.
--
-- This is an infix alias for 'cons'.
--
-- >>> a <| []
-- [a]
--
-- >>> a <| [b, c]
-- [a,b,c]
--
-- >>> a <| Seq.fromList []
-- fromList [a]
--
-- >>> a <| Seq.fromList [b, c]
-- fromList [a,b,c]
(<|) :: Cons s s a a => a -> s -> s
(<|) = curry (simply review _Cons)
{-# INLINE (<|) #-}

-- | 'cons' an element onto a container.
--
-- >>> cons a []
-- [a]
--
-- >>> cons a [b, c]
-- [a,b,c]
--
-- >>> cons a (Seq.fromList [])
-- fromList [a]
--
-- >>> cons a (Seq.fromList [b, c])
-- fromList [a,b,c]
cons :: Cons s s a a => a -> s -> s
cons = curry (simply review _Cons)
{-# INLINE cons #-}

-- | Attempt to extract the left-most element from a container, and a version of the container without that element.
--
-- >>> uncons []
-- Nothing
--
-- >>> uncons [a, b, c]
-- Just (a,[b,c])
uncons :: Cons s s a a => s -> Maybe (a, s)
uncons = simply preview _Cons
{-# INLINE uncons #-}

-- | A 'Traversal' reading and writing to the 'head' of a /non-empty/ container.
--
-- >>> [a,b,c]^? _head
-- Just a
--
-- >>> [a,b,c] & _head .~ d
-- [d,b,c]
--
-- >>> [a,b,c] & _head %~ f
-- [f a,b,c]
--
-- >>> [] & _head %~ f
-- []
--
-- >>> [1,2,3]^?!_head
-- 1
--
-- >>> []^?_head
-- Nothing
--
-- >>> [1,2]^?_head
-- Just 1
--
-- >>> [] & _head .~ 1
-- []
--
-- >>> [0] & _head .~ 2
-- [2]
--
-- >>> [0,1] & _head .~ 2
-- [2,1]
--
-- This isn't limited to lists.
--
-- For instance you can also 'Data.Traversable.traverse' the head of a 'Seq':
--
-- >>> Seq.fromList [a,b,c,d] & _head %~ f
-- fromList [f a,b,c,d]
--
-- >>> Seq.fromList [] ^? _head
-- Nothing
--
-- >>> Seq.fromList [a,b,c,d] ^? _head
-- Just a
--
-- @
-- '_head' :: 'Traversal'' [a] a
-- '_head' :: 'Traversal'' ('Seq' a) a
-- '_head' :: 'Traversal'' ('Vector' a) a
-- @
_head :: Cons s s a a => Traversal' s a
_head = _Cons._1
{-# INLINE _head #-}

-- | A 'Traversal' reading and writing to the 'tail' of a /non-empty/ container.
--
-- >>> [a,b] & _tail .~ [c,d,e]
-- [a,c,d,e]
--
-- >>> [] & _tail .~ [a,b]
-- []
--
-- >>> [a,b,c,d,e] & _tail.traverse %~ f
-- [a,f b,f c,f d,f e]
--
-- >>> [1,2] & _tail .~ [3,4,5]
-- [1,3,4,5]
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> [a,b,c]^?_tail
-- Just [b,c]
--
-- >>> [1,2]^?!_tail
-- [2]
--
-- >>> "hello"^._tail
-- "ello"
--
-- >>> ""^._tail
-- ""
--
-- This isn't limited to lists. For instance you can also 'Control.Traversable.traverse' the tail of a 'Seq'.
--
-- >>> Seq.fromList [a,b] & _tail .~ Seq.fromList [c,d,e]
-- fromList [a,c,d,e]
--
-- >>> Seq.fromList [a,b,c] ^? _tail
-- Just (fromList [b,c])
--
-- >>> Seq.fromList [] ^? _tail
-- Nothing
--
-- @
-- '_tail' :: 'Traversal'' [a] [a]
-- '_tail' :: 'Traversal'' ('Seq' a) ('Seq' a)
-- '_tail' :: 'Traversal'' ('Vector' a) ('Vector' a)
-- @
_tail :: Cons s s a a => Traversal' s s
_tail = _Cons._2
{-# INLINE _tail #-}

-- | Modify the target of a 'Cons' value by using @('<|')@.
--
-- >>> (["world"], ["lens"]) & _1 <|~ "hello"
-- (["hello","world"],["lens"])
(<|~) :: Cons b b a a => ASetter s t b b -> a -> s -> t
l <|~ n = over l (n <|)
{-# INLINE (<|~) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by using @('<|')@.
(<|=) :: (MonadState s m, Cons b b a a) => ASetter s s b b -> a -> m ()
l <|= a = State.modify (l <|~ a)
{-# INLINE (<|=) #-}

-- | ('<|') a 'Cons' value onto the end of the target of a 'Lens' and
-- return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Cons.<|~') is more flexible.
(<<|~) :: Cons b b a a => LensLike ((,) b) s t b b -> a -> s -> (b, t)
l <<|~ m = l <%~ (m <|)
{-# INLINE (<<|~) #-}

-- | ('<|') a 'Semigroup' value onto the end of the target of a 'Lens' into
-- your 'Monad''s state and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Cons.<|=') is more flexible.
(<<|=) :: (MonadState s m, Cons b b a a) => LensLike ((,) b) s s b b -> a -> m b
l <<|= r = l <%= (r <|)
{-# INLINE (<<|=) #-}

------------------------------------------------------------------------------
-- Snoc
------------------------------------------------------------------------------

-- | This class provides a way to attach or detach elements on the right
-- side of a structure in a flexible manner.
class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- |
  --
  -- @
  -- '_Snoc' :: 'Prism' [a] [b] ([a], a) ([b], b)
  -- '_Snoc' :: 'Prism' ('Seq' a) ('Seq' b) ('Seq' a, a) ('Seq' b, b)
  -- '_Snoc' :: 'Prism' ('Vector' a) ('Vector' b) ('Vector' a, a) ('Vector' b, b)
  -- '_Snoc' :: 'Prism'' 'String' ('String', 'Char')
  -- '_Snoc' :: 'Prism'' 'StrictT.Text' ('StrictT.Text', 'Char')
  -- '_Snoc' :: 'Prism'' 'StrictB.ByteString' ('StrictB.ByteString', 'Word8')
  -- @
  _Snoc :: Prism s t (s,a) (t,b)

instance Snoc [a] [b] a b where
  _Snoc = prism (\(as,a) -> as Prelude.++ [a]) $ \aas -> if Prelude.null aas
    then Left []
    else Right (Prelude.init aas, Prelude.last aas)
  {-# INLINE _Snoc #-}

instance Snoc (ZipList a) (ZipList b) a b where
  _Snoc = withPrism listSnoc $ \listReview listPreview ->
    prism (coerce listReview) (coerce listPreview) where

    listSnoc :: Prism [a] [b] ([a], a) ([b], b)
    listSnoc = _Snoc

  {-# INLINE _Snoc #-}

instance Snoc (Seq a) (Seq b) a b where
  _Snoc = prism (uncurry (Seq.|>)) $ \aas -> case viewr aas of
    as Seq.:> a -> Right (as, a)
    EmptyR  -> Left mempty
  {-# INLINE _Snoc #-}

instance Snoc (Vector a) (Vector b) a b where
  _Snoc = prism (uncurry Vector.snoc) $ \v -> if Vector.null v
    then Left Vector.empty
    else Right (Vector.unsafeInit v, Vector.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Prim a, Prim b) => Snoc (Prim.Vector a) (Prim.Vector b) a b where
  _Snoc = prism (uncurry Prim.snoc) $ \v -> if Prim.null v
    then Left Prim.empty
    else Right (Prim.unsafeInit v, Prim.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Storable a, Storable b) => Snoc (Storable.Vector a) (Storable.Vector b) a b where
  _Snoc = prism (uncurry Storable.snoc) $ \v -> if Storable.null v
    then Left Storable.empty
    else Right (Storable.unsafeInit v, Storable.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Unbox a, Unbox b) => Snoc (Unbox.Vector a) (Unbox.Vector b) a b where
  _Snoc = prism (uncurry Unbox.snoc) $ \v -> if Unbox.null v
    then Left Unbox.empty
    else Right (Unbox.unsafeInit v, Unbox.unsafeLast v)
  {-# INLINE _Snoc #-}

instance Snoc StrictB.ByteString StrictB.ByteString Word8 Word8 where
  _Snoc = prism (uncurry StrictB.snoc) $ \v -> if StrictB.null v
    then Left StrictB.empty
    else Right (StrictB.init v, StrictB.last v)
  {-# INLINE _Snoc #-}

instance Snoc LazyB.ByteString LazyB.ByteString Word8 Word8 where
  _Snoc = prism (uncurry LazyB.snoc) $ \v -> if LazyB.null v
    then Left LazyB.empty
    else Right (LazyB.init v, LazyB.last v)
  {-# INLINE _Snoc #-}

instance Snoc StrictT.Text StrictT.Text Char Char where
  _Snoc = prism (uncurry StrictT.snoc) $ \v -> if StrictT.null v
    then Left StrictT.empty
    else Right (StrictT.init v, StrictT.last v)
  {-# INLINE _Snoc #-}

instance Snoc LazyT.Text LazyT.Text Char Char where
  _Snoc = prism (uncurry LazyT.snoc) $ \v -> if LazyT.null v
    then Left LazyT.empty
    else Right (LazyT.init v, LazyT.last v)
  {-# INLINE _Snoc #-}

-- | A 'Traversal' reading and replacing all but the a last element of a /non-empty/ container.
--
-- >>> [a,b,c,d]^?_init
-- Just [a,b,c]
--
-- >>> []^?_init
-- Nothing
--
-- >>> [a,b] & _init .~ [c,d,e]
-- [c,d,e,b]
--
-- >>> [] & _init .~ [a,b]
-- []
--
-- >>> [a,b,c,d] & _init.traverse %~ f
-- [f a,f b,f c,d]
--
-- >>> [1,2,3]^?_init
-- Just [1,2]
--
-- >>> [1,2,3,4]^?!_init
-- [1,2,3]
--
-- >>> "hello"^._init
-- "hell"
--
-- >>> ""^._init
-- ""
--
-- @
-- '_init' :: 'Traversal'' [a] [a]
-- '_init' :: 'Traversal'' ('Seq' a) ('Seq' a)
-- '_init' :: 'Traversal'' ('Vector' a) ('Vector' a)
-- @
_init :: Snoc s s a a => Traversal' s s
_init = _Snoc._1
{-# INLINE _init #-}

-- | A 'Traversal' reading and writing to the last element of a /non-empty/ container.
--
-- >>> [a,b,c]^?!_last
-- c
--
-- >>> []^?_last
-- Nothing
--
-- >>> [a,b,c] & _last %~ f
-- [a,b,f c]
--
-- >>> [1,2]^?_last
-- Just 2
--
-- >>> [] & _last .~ 1
-- []
--
-- >>> [0] & _last .~ 2
-- [2]
--
-- >>> [0,1] & _last .~ 2
-- [0,2]
--
-- This 'Traversal' is not limited to lists, however. We can also work with other containers, such as a 'Vector'.
--
-- >>> Vector.fromList "abcde" ^? _last
-- Just 'e'
--
-- >>> Vector.empty ^? _last
-- Nothing
--
-- >>> (Vector.fromList "abcde" & _last .~ 'Q') == Vector.fromList "abcdQ"
-- True
--
-- @
-- '_last' :: 'Traversal'' [a] a
-- '_last' :: 'Traversal'' ('Seq' a) a
-- '_last' :: 'Traversal'' ('Vector' a) a
-- @
_last :: Snoc s s a a => Traversal' s a
_last = _Snoc._2
{-# INLINE _last #-}

-- | 'snoc' an element onto the end of a container.
--
-- This is an infix alias for 'snoc'.
--
-- >>> Seq.fromList [] |> a
-- fromList [a]
--
-- >>> Seq.fromList [b, c] |> a
-- fromList [b,c,a]
--
-- >>> LazyT.pack "hello" |> '!'
-- "hello!"
(|>) :: Snoc s s a a => s -> a -> s
(|>) = curry (simply review _Snoc)
{-# INLINE (|>) #-}

-- | 'snoc' an element onto the end of a container.
--
-- >>> snoc (Seq.fromList []) a
-- fromList [a]
--
-- >>> snoc (Seq.fromList [b, c]) a
-- fromList [b,c,a]
--
-- >>> snoc (LazyT.pack "hello") '!'
-- "hello!"
snoc  :: Snoc s s a a => s -> a -> s
snoc = curry (simply review _Snoc)
{-# INLINE snoc #-}

-- | Attempt to extract the right-most element from a container, and a version of the container without that element.
--
-- >>> unsnoc (LazyT.pack "hello!")
-- Just ("hello",'!')
--
-- >>> unsnoc (LazyT.pack "")
-- Nothing
--
-- >>> unsnoc (Seq.fromList [b,c,a])
-- Just (fromList [b,c],a)
--
-- >>> unsnoc (Seq.fromList [])
-- Nothing
unsnoc :: Snoc s s a a => s -> Maybe (s, a)
unsnoc = simply preview _Snoc
{-# INLINE unsnoc #-}

-- | Modify the target of a 'Cons' value by using @('|>')@.
--
-- >>> (["world"], ["lens"]) & _1 |>~ "hello"
-- (["world","hello"],["lens"])
(|>~) :: Snoc b b a a => ASetter s t b b -> a -> s -> t
l |>~ n = over l (|> n)
{-# INLINE (|>~) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by using @('|>')@.
(|>=) :: (MonadState s m, Snoc b b a a) => ASetter s s b b -> a -> m ()
l |>= a = State.modify (l |>~ a)
{-# INLINE (|>=) #-}

-- | ('|>') a 'Cons' value onto the end of the target of a 'Lens' and
-- return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Cons.|>~') is more flexible.
(<|>~) :: Snoc b b p p => LensLike ((,) b) s t b b -> p -> s -> (b, t)
l <|>~ m = l <%~ (|> m)
{-# INLINE (<|>~) #-}

-- | ('|>') a 'Semigroup' value onto the end of the target of a 'Lens' into
-- your 'Monad''s state and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Cons.|>=') is more flexible.
(<|>=) :: (MonadState s m, Snoc b b p p) => LensLike ((,) b) s s b b -> p -> m b
l <|>= r = l <%= (|> r)
{-# INLINE (<|>=) #-}
