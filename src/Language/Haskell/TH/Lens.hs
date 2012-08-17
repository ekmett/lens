{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
-- Lenses and Traversals for working with Template Haskell
----------------------------------------------------------------------------
module Language.Haskell.TH.Lens
  ( HasName(..)
  , HasTypeVars(..)
  , SubstType(..)
  , typeVars      -- :: HasTypeVars t => Simple Traversal t Name
  , substTypeVars -- :: HasTypeVars t => Map Name Name -> t -> t
  , conFields
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Type
import Control.Lens.Traversal
import Data.Map as Map hiding (toList,map)
import Data.Map.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set as Set hiding (toList,map)
import Data.Set.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Simple Lens t Name

instance HasName TyVarBndr where
  name f (PlainTV n) = PlainTV <$> f n
  name f (KindedTV n k) = (`KindedTV` k) <$> f n

instance HasName Name where
  name = id

instance HasName Con where
  name f (NormalC n tys)       = (`NormalC` tys) <$> f n
  name f (RecC n tys)          = (`RecC` tys) <$> f n
  name f (InfixC l n r)        = (\n' -> InfixC l n' r) <$> f n
  name f (ForallC bds ctx con) = ForallC bds ctx <$> name f con

-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- | When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Simple Traversal t Name

instance HasTypeVars TyVarBndr where
  typeVarsEx s f b
    | s^.contains (b^.name) = pure b
    | otherwise             = name f b

instance HasTypeVars Name where
  typeVarsEx s f n
    | s^.contains n = pure n
    | otherwise     = f n

instance HasTypeVars Type where
  typeVarsEx s f (VarT n)            = VarT <$> typeVarsEx s f n
  typeVarsEx s f (AppT l r)          = AppT <$> typeVarsEx s f l <*> typeVarsEx s f r
  typeVarsEx s f (SigT t k)          = (`SigT` k) <$> typeVarsEx s f t
  typeVarsEx s f (ForallT bs ctx ty) = ForallT bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f ty
       where s' = s `Set.union` setOf typeVars bs
  typeVarsEx _ _ t                   = pure t

instance HasTypeVars Pred where
  typeVarsEx s f (ClassP n ts) = ClassP n <$> typeVarsEx s f ts
  typeVarsEx s f (EqualP l r)  = EqualP <$> typeVarsEx s f l <*> typeVarsEx s f r

instance HasTypeVars t => HasTypeVars [t] where
  typeVarsEx s = traverse . typeVarsEx s

-- | Traverse /free/ type variables
typeVars :: HasTypeVars t => Simple Traversal t Name
typeVars = typeVarsEx mempty

-- | Substitute using a map of names in for /free/ type variables
substTypeVars :: HasTypeVars t => Map Name Name -> t -> t
substTypeVars m = mapOf typeVars $ \n -> fromMaybe n (m^.at n)

-- | Provides substitution for types
class SubstType t where
  -- | Perform substitution for types
  substType :: Map Name Type -> t -> t

instance SubstType Type where
  substType m t@(VarT n)          = fromMaybe t (m^.at n)
  substType m (ForallT bs ctx ty) = ForallT bs (substType m ctx) (substType m ty)
  substType m (SigT t k)          = SigT (substType m t) k
  substType m (AppT l r)          = AppT (substType m l) (substType m r)
  substType _ t                   = t

instance SubstType t => SubstType [t] where
  substType = map . substType

instance SubstType Pred where
  substType m (ClassP n ts) = ClassP n (substType m ts)
  substType m (EqualP l r)  = substType m (EqualP l r)

conFields :: Simple Traversal Con StrictType
conFields f (NormalC n tys)     = NormalC n <$> traverse f tys
conFields f (RecC n tys)        = RecC n <$> traverse sans_var tys
  where sans_var (fn,s,t) = (\(s', t') -> (fn,s',t')) <$> f (s, t)
conFields f (InfixC l n r)      = InfixC <$> f l <*> pure n <*> f r
conFields f (ForallC bds ctx c) = ForallC bds ctx <$> conFields f c
