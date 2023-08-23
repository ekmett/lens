-- | This module exists for the sole purpose of redefining the 'head' and 'tail'
-- functions (which are normally provided by the 'Prelude') so that they can be
-- used in the doctests of 'Data.Data.Lens'.
--
-- The 'head' and 'tail' functions are partial, and as of GHC 9.8, there is a
-- @-Wx-partial@ warning (implied by @-Wall@) that triggers any time you use
-- either of these functions. This is a fairly reasonable default in most
-- settings, but there are a handful of doctests in 'Data.Data.Lens' that do in
-- fact rely on 'head' and 'tail' being partial functions. These doctests
-- demonstrate that various functions in 'Data.Data.Lens' can recover from
-- exceptions that are thrown due to partiality (see, for instance, the @upon@
-- function).
--
-- One possible workaround would be to disable @-Wx-partial@. We don't want to
-- disable the warning for /all/ code in @lens@, however—we only want to
-- disable it for a particular group of doctests. It is rather tricky to achieve
-- this level of granularity, unfortunately. This is because tools like
-- @cabal-docspec@ rely on GHCi to work, and the statefulness of GHCi's @:set@
-- command means that disabling @-Wx-partial@ might leak into other modules'
-- doctests, which we don't want.
--
-- Instead, we opt to redefine our own versions of 'head' and 'tail' here, which
-- do not trigger any @-Wx-partial@ warnings, and use them in the
-- 'Data.Data.Lens' doctests. This has no impact on anyone reading the doctests,
-- as these functions will look indistinguishable from the 'head' and 'tail'
-- functions in the 'Prelude'. One consequence of this design is that we must
-- export the 'Control.Lens.Internal.Doctest' module, as GHCi (and therefore
-- @cabal-docspec@) won't be able to import it otherwise. Despite this technical
-- oddity, this module should be thought of as internal to @lens@.
module Control.Lens.Internal.Doctest
  ( head
  , tail
  ) where

import Prelude hiding (head, tail)

head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "tail: empty list"
