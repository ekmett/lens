{-# LANGUAGE Rank2Types #-}
module Control.Lens.Reified where

import Control.Lens.Type

------------------------------------------------------------------------------
-- Reifying
------------------------------------------------------------------------------

-- | Useful for storing lenses in containers.
newtype ReifiedLens s t a b = ReifyLens { reflectLens :: Lens s t a b }

-- | @type 'ReifiedLens'' = 'Simple' 'ReifiedLens'@
type ReifiedLens' s a = ReifiedLens s s a a

-- | Useful for storage.
newtype ReifiedIndexedLens i s t a b = ReifyIndexedLens { reflectIndexedLens :: IndexedLens i s t a b }

-- | @type 'ReifiedIndexedLens'' i = 'Simple' ('ReifiedIndexedLens' i)@
type ReifiedIndexedLens' i s a = ReifiedIndexedLens i s s a a

-- | Useful for storage.
newtype ReifiedIndexedTraversal i s t a b = ReifyIndexedTraversal { reflectIndexedTraversal :: IndexedTraversal i s t a b }

-- | @type 'ReifiedIndexedTraversal'' i = 'Simple' ('ReifiedIndexedTraversal' i)@
type ReifiedIndexedTraversal' i s a = ReifiedIndexedTraversal i s s a a

-- | A form of 'Traversal' that can be stored monomorphically in a container.
data ReifiedTraversal s t a b = ReifyTraversal { reflectTraversal :: Traversal s t a b }

-- | @type 'ReifiedTraversal'' = 'Simple' 'ReifiedTraversal'@
type ReifiedTraversal' s a = ReifiedTraversal s s a a

-- | Useful for storing getters in containers.
newtype ReifiedGetter s a = ReifyGetter { reflectGetter :: Getter s a }

-- | Useful for storage.
newtype ReifiedIndexedGetter i s a = ReifyIndexedGetter { reflectIndexedGetter :: IndexedGetter i s a }

-- | Useful for storing folds in containers.
newtype ReifiedFold s a = ReifyFold { reflectFold :: Fold s a }

-- | Reify a setter so it can be stored safely in a container.
newtype ReifiedSetter s t a b = ReifySetter { reflectSetter :: Setter s t a b }

-- | @type 'ReifiedSetter'' = 'Simple' 'ReifiedSetter'@
type ReifiedSetter' s a = ReifiedSetter s s a a

-- | Reify an indexed setter so it can be stored safely in a container.
newtype ReifiedIndexedSetter i s t a b =
  ReifyIndexedSetter { reflectIndexedSetter :: IndexedSetter i s t a b }

-- | @type 'ReifiedIndexedSetter'' i = 'Simple' ('ReifiedIndexedSetter' i)@
type ReifiedIndexedSetter' i s a = ReifiedIndexedSetter i s s a a

-- | Reify a setter so it can be stored safely in a container.
newtype ReifiedIso s t a b = ReifyIso { reflectIso :: Iso s t a b }

-- | @type 'ReifiedIso'' = 'Simple' 'ReifiedIso'@
type ReifiedIso' s a = ReifiedIso s s a a

-- | Reify a prism so it can be stored safely in a container.
newtype ReifiedPrism s t a b = ReifyPrism { reflectPrism :: Prism s t a b }

-- | @type 'ReifiedPrism'' = 'Simple' 'ReifiedPrism'@
type ReifiedPrism' s a = ReifiedPrism s s a a
