{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE Rank2Types #-}

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
-- Lenses, Prisms, and Traversals for working with Template Haskell.
--
-- Beware that the API offered in this module is subject to change, as it
-- mirrors the API exposed by the @template-haskell@ package, which
-- frequently changes between different releases of GHC. An effort is made
-- to identify the functions in this module which have different type
-- signatures when compiled with different versions of @template-haskell@.
----------------------------------------------------------------------------
module Language.Haskell.TH.Lens
  (
  -- * Traversals
    HasName(..)
  , HasTypes(..)
  , HasTypeVars(..)
  , SubstType(..)
  , typeVars      -- :: HasTypeVars t => Traversal' t Name
  , substTypeVars -- :: HasTypeVars t => Map Name Name -> t -> t
  , conFields
  , conNamedFields
  -- * Lenses
  -- ** Loc Lenses
  , locFileName
  , locPackage
  , locModule
  , locStart
  , locEnd
  -- ** FunDep Lenses
  , funDepInputs
  , funDepOutputs
  -- ** Match Lenses
  , matchPattern
  , matchBody
  , matchDeclarations
  -- ** Fixity Lenses
  , fixityPrecedence
  , fixityDirection
  -- ** Clause Lenses
  , clausePattern
  , clauseBody
  , clauseDecs
  -- ** FieldExp Lenses
  , fieldExpName
  , fieldExpExpression
  -- ** FieldPat Lenses
  , fieldPatName
  , fieldPatPattern
  -- ** TySynEqn Lenses
# if MIN_VERSION_template_haskell(2,15,0)
  , tySynEqnLHS
# endif
  , tySynEqnPatterns
  , tySynEqnResult
  -- ** InjectivityAnn Lenses
  , injectivityAnnOutput
  , injectivityAnnInputs
  -- ** TypeFamilyHead Lenses
  , typeFamilyHeadName
  , typeFamilyHeadTyVarBndrs
  , typeFamilyHeadResultSig
  , typeFamilyHeadInjectivityAnn
  -- ** Bang Lenses
  , bangSourceUnpackedness
  , bangSourceStrictness
#if MIN_VERSION_template_haskell(2,12,0)
  -- ** DerivClause Lenses
  , derivClauseStrategy
  , derivClauseCxt
#endif
  -- * Prisms
  -- ** Info Prisms
  , _ClassI
  , _ClassOpI
  , _TyConI
  , _FamilyI
  , _PrimTyConI
  , _DataConI
  , _VarI
  , _TyVarI
#if MIN_VERSION_template_haskell(2,12,0)
  , _PatSynI
#endif
  -- ** Dec Prisms
  , _FunD
  , _ValD
  , _DataD
  , _NewtypeD
  , _TySynD
  , _ClassD
  , _InstanceD
  , _SigD
  , _ForeignD
  , _InfixD
  , _PragmaD
  , _DataInstD
  , _NewtypeInstD
  , _TySynInstD
  , _ClosedTypeFamilyD
  , _RoleAnnotD
  , _StandaloneDerivD
  , _DefaultSigD
  , _DataFamilyD
  , _OpenTypeFamilyD
#if MIN_VERSION_template_haskell(2,12,0)
  , _PatSynD
  , _PatSynSigD
#endif
#if MIN_VERSION_template_haskell(2,15,0)
  , _ImplicitParamBindD
#endif
#if MIN_VERSION_template_haskell(2,12,0)
  -- ** PatSynDir Prisms
  , _Unidir
  , _ImplBidir
  , _ExplBidir
  -- ** PatSynArgs Prisms
  , _PrefixPatSyn
  , _InfixPatSyn
  , _RecordPatSyn
#endif
  -- ** Con Prisms
  , _NormalC
  , _RecC
  , _InfixC
  , _ForallC
  , _GadtC
  , _RecGadtC
  -- ** Overlap Prisms
  ,_Overlappable
  ,_Overlapping
  ,_Overlaps
  ,_Incoherent
  -- ** SourceUnpackedness Prisms
  , _NoSourceUnpackedness
  , _SourceNoUnpack
  , _SourceUnpack
  -- ** SourceStrictness Prisms
  , _NoSourceStrictness
  , _SourceLazy
  , _SourceStrict
  -- ** DecidedStrictness Prisms
  , _DecidedLazy
  , _DecidedStrict
  , _DecidedUnpack
  -- ** Foreign Prisms
  , _ImportF
  , _ExportF
  -- ** Callconv Prisms
  , _CCall
  , _StdCall
  , _CApi
  , _Prim
  , _JavaScript
  -- ** Safety Prisms
  , _Unsafe
  , _Safe
  , _Interruptible
  -- ** Pragma Prisms
  , _InlineP
  , _SpecialiseP
  , _SpecialiseInstP
  , _RuleP
  , _AnnP
  , _LineP
#if MIN_VERSION_template_haskell(2,12,0)
  , _CompleteP
#endif
  -- ** Inline Prisms
  , _NoInline
  , _Inline
  , _Inlinable
  -- ** RuleMatch Prisms
  , _ConLike
  , _FunLike
  -- ** Phases Prisms
  , _AllPhases
  , _FromPhase
  , _BeforePhase
  -- ** RuleBndr Prisms
  , _RuleVar
  , _TypedRuleVar
  -- ** AnnTarget Prisms
  , _ModuleAnnotation
  , _TypeAnnotation
  , _ValueAnnotation
  -- ** FunDep Prisms TODO make a lens
  , _FunDep
#if !(MIN_VERSION_template_haskell(2,13,0))
  -- ** FamFlavour Prisms
  , _TypeFam
  , _DataFam
#endif
  -- ** FixityDirection Prisms
  , _InfixL
  , _InfixR
  , _InfixN
  -- ** Exp Prisms
  , _VarE
  , _ConE
  , _LitE
  , _AppE
#if MIN_VERSION_template_haskell(2,12,0)
  , _AppTypeE
#endif
  , _InfixE
  , _UInfixE
  , _ParensE
  , _LamE
  , _LamCaseE
  , _TupE
  , _UnboxedTupE
#if MIN_VERSION_template_haskell(2,12,0)
  , _UnboxedSumE
#endif
  , _CondE
  , _MultiIfE
  , _LetE
  , _CaseE
  , _DoE
  , _CompE
  , _ArithSeqE
  , _ListE
  , _SigE
  , _RecConE
  , _RecUpdE
  , _StaticE
  , _UnboundVarE
#if MIN_VERSION_template_haskell(2,13,0)
  , _LabelE
#endif
#if MIN_VERSION_template_haskell(2,15,0)
  , _MDoE
  , _ImplicitParamVarE
#endif
  -- ** Body Prisms
  , _GuardedB
  , _NormalB
  -- ** Guard Prisms
  , _NormalG
  , _PatG
  -- ** Stmt Prisms
  , _BindS
  , _LetS
  , _NoBindS
  , _ParS
#if MIN_VERSION_template_haskell(2,15,0)
  , _RecS
#endif
  -- ** Range Prisms
  , _FromR
  , _FromThenR
  , _FromToR
  , _FromThenToR
  -- ** Lit Prisms
  , _CharL
  , _StringL
  , _IntegerL
  , _RationalL
  , _IntPrimL
  , _WordPrimL
  , _FloatPrimL
  , _DoublePrimL
  , _StringPrimL
  , _CharPrimL
#if MIN_VERSION_template_haskell(2,16,0)
  , _BytesPrimL
#endif
  -- ** Pat Prisms
  , _LitP
  , _VarP
  , _TupP
  , _UnboxedTupP
#if MIN_VERSION_template_haskell(2,12,0)
  , _UnboxedSumP
#endif
  , _ConP
  , _InfixP
  , _UInfixP
  , _ParensP
  , _TildeP
  , _BangP
  , _AsP
  , _WildP
  , _RecP
  , _ListP
  , _SigP
  , _ViewP
  -- ** Type Prisms
  , _ForallT
  , _AppT
  , _SigT
  , _VarT
  , _ConT
  , _PromotedT
  , _TupleT
  , _UnboxedTupleT
#if MIN_VERSION_template_haskell(2,12,0)
  , _UnboxedSumT
#endif
  , _ArrowT
  , _EqualityT
  , _ListT
  , _PromotedTupleT
  , _PromotedNilT
  , _PromotedConsT
  , _StarT
  , _ConstraintT
  , _LitT
  , _InfixT
  , _UInfixT
  , _ParensT
  , _WildCardT
#if MIN_VERSION_template_haskell(2,15,0)
  , _AppKindT
  , _ImplicitParamT
#endif
#if MIN_VERSION_template_haskell(2,16,0)
  , _ForallVisT
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  , _MulArrowT
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  -- ** Specificity Prisms
  , _SpecifiedSpec
  , _InferredSpec
#endif
  -- ** TyVarBndr Prisms
  , _PlainTV
  , _KindedTV
  -- ** FamilyResultSig Prisms
  , _NoSig
  , _KindSig
  , _TyVarSig
  -- ** TyLit Prisms
  , _NumTyLit
  , _StrTyLit
#if MIN_VERSION_template_haskell(2,18,0)
  , _CharTyLit
#endif
  -- ** Role Prisms
  , _NominalR
  , _RepresentationalR
  , _PhantomR
  , _InferR
#if MIN_VERSION_template_haskell(2,12,0)
  -- ** DerivStrategy Prisms
  , _StockStrategy
  , _AnyclassStrategy
  , _NewtypeStrategy
#endif
  ) where

import Control.Applicative
import Control.Lens.At
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Fold
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.Tuple
import Control.Lens.Traversal
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Set.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
import Language.Haskell.TH.Syntax
import Data.Word
#if MIN_VERSION_template_haskell(2,15,0)
import Control.Lens.Internal.TH (unfoldType)
import Data.Foldable as F (foldl')
#endif
import Prelude

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Lens' t Name

instance HasName (TyVarBndr_ flag) where
  name = traverseTVName

instance HasName Name where
  name = id

-- | On @template-haskell-2.11.0.0@ or later, if a 'GadtC' or 'RecGadtC' has
-- multiple 'Name's, the leftmost 'Name' will be chosen.
instance HasName Con where
  name f (NormalC n tys)       = (`NormalC` tys) <$> f n
  name f (RecC n tys)          = (`RecC` tys) <$> f n
  name f (InfixC l n r)        = (\n' -> InfixC l n' r) <$> f n
  name f (ForallC bds ctx con) = ForallC bds ctx <$> name f con
  name f (GadtC ns argTys retTy) =
    (\n -> GadtC [n] argTys retTy) <$> f (head ns)
  name f (RecGadtC ns argTys retTy) =
    (\n -> RecGadtC [n] argTys retTy) <$> f (head ns)

instance HasName Foreign where
  name f (ImportF cc saf str n ty) =
    (\n' -> ImportF cc saf str n' ty) <$> f n
  name f (ExportF cc str n ty) =
    (\n' -> ExportF cc str n' ty) <$> f n

instance HasName RuleBndr where
  name f (RuleVar n) = RuleVar <$> f n
  name f (TypedRuleVar n ty) = (`TypedRuleVar` ty) <$> f n

instance HasName TypeFamilyHead where
  name f (TypeFamilyHead n tvbs frs mia) =
    (\n' -> TypeFamilyHead n' tvbs frs mia) <$> f n

instance HasName InjectivityAnn where
  name f (InjectivityAnn n deps) = (`InjectivityAnn` deps) <$> f n

-- | Contains some amount of `Type`s inside
class HasTypes t where
  -- | Traverse all the types
  types :: Traversal' t Type

instance HasTypes Type where
  types = id

instance HasTypes Con where
  types f (NormalC n t)      = NormalC n <$> traverse (_2 (types f)) t
  types f (RecC n t)         = RecC n <$> traverse (_3 (types f)) t
  types f (InfixC t1 n t2) = InfixC <$> _2 (types f) t1
                                       <*> pure n <*> _2 (types f) t2
  types f (ForallC vb ctx con)    = ForallC vb ctx <$> types f con
  types f (GadtC ns argTys retTy) =
    GadtC    ns <$> traverse (_2 (types f)) argTys <*> types f retTy
  types f (RecGadtC ns argTys retTy) =
    RecGadtC ns <$> traverse (_3 (types f)) argTys <*> types f retTy

instance HasTypes Foreign where
  types f (ImportF cc saf str n t) = ImportF cc saf str n <$> types f t
  types f (ExportF cc     str n t) = ExportF cc     str n <$> types f t

instance HasTypes TySynEqn where
#if MIN_VERSION_template_haskell(2,15,0)
  types f (TySynEqn mtvbs lhs rhs) = TySynEqn <$> traverse (traverse go) mtvbs
                                              <*> types f lhs
                                              <*> types f rhs
    where
      go = traverseTVKind f
#else
  types f (TySynEqn lhss rhs) = TySynEqn <$> traverse (types f) lhss
                                         <*> types f rhs
#endif

instance HasTypes t => HasTypes [t] where
  types = traverse . types

-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- | When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

instance HasTypeVars (TyVarBndr_ flag) where
  typeVarsEx s f b
    | s^.contains (b^.name) = pure b
    | otherwise             = name f b

instance HasTypeVars Name where
  typeVarsEx s f n
    | s^.contains n = pure n
    | otherwise     = f n

instance HasTypeVars Type where
  typeVarsEx s f (VarT n)             = VarT <$> typeVarsEx s f n
  typeVarsEx s f (AppT l r)           = AppT <$> typeVarsEx s f l <*> typeVarsEx s f r
  typeVarsEx s f (ForallT bs ctx ty)  = ForallT bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f ty
       where s' = s `Set.union` setOf typeVars bs
  typeVarsEx _ _ t@ConT{}             = pure t
  typeVarsEx _ _ t@TupleT{}           = pure t
  typeVarsEx _ _ t@ListT{}            = pure t
  typeVarsEx _ _ t@ArrowT{}           = pure t
  typeVarsEx _ _ t@UnboxedTupleT{}    = pure t
  typeVarsEx s f (SigT t k)           = SigT <$> typeVarsEx s f t
                                             <*> typeVarsEx s f k
  typeVarsEx _ _ t@PromotedT{}        = pure t
  typeVarsEx _ _ t@PromotedTupleT{}   = pure t
  typeVarsEx _ _ t@PromotedNilT{}     = pure t
  typeVarsEx _ _ t@PromotedConsT{}    = pure t
  typeVarsEx _ _ t@StarT{}            = pure t
  typeVarsEx _ _ t@ConstraintT{}      = pure t
  typeVarsEx _ _ t@LitT{}             = pure t
  typeVarsEx _ _ t@EqualityT{}        = pure t
  typeVarsEx s f (InfixT  t1 n t2)    = InfixT  <$> typeVarsEx s f t1
                                                <*> pure n
                                                <*> typeVarsEx s f t2
  typeVarsEx s f (UInfixT t1 n t2)    = UInfixT <$> typeVarsEx s f t1
                                                <*> pure n
                                                <*> typeVarsEx s f t2
  typeVarsEx s f (ParensT t)          = ParensT <$> typeVarsEx s f t
  typeVarsEx _ _ t@WildCardT{}        = pure t
#if MIN_VERSION_template_haskell(2,12,0)
  typeVarsEx _ _ t@UnboxedSumT{}      = pure t
#endif
#if MIN_VERSION_template_haskell(2,15,0)
  typeVarsEx s f (AppKindT t k)       = AppKindT <$> typeVarsEx s f t
                                                 <*> typeVarsEx s f k
  typeVarsEx s f (ImplicitParamT n t) = ImplicitParamT n <$> typeVarsEx s f t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
  typeVarsEx s f (ForallVisT bs ty)   = ForallVisT bs <$> typeVarsEx s' f ty
       where s' = s `Set.union` setOf typeVars bs
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  typeVarsEx _ _ t@MulArrowT{}        = pure t
#endif

instance HasTypeVars Con where
  typeVarsEx s f (NormalC n ts) = NormalC n <$> traverseOf (traverse . _2) (typeVarsEx s f) ts
  typeVarsEx s f (RecC n ts) = RecC n <$> traverseOf (traverse . _3) (typeVarsEx s f) ts
  typeVarsEx s f (InfixC l n r) = InfixC <$> g l <*> pure n <*> g r
       where g (i, t) = (,) i <$> typeVarsEx s f t
  typeVarsEx s f (ForallC bs ctx c) = ForallC bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f c
       where s' = s `Set.union` setOf typeVars bs
  typeVarsEx s f (GadtC ns argTys retTy) =
    GadtC ns <$> traverseOf (traverse . _2) (typeVarsEx s f) argTys
             <*> typeVarsEx s f retTy
  typeVarsEx s f (RecGadtC ns argTys retTy) =
    RecGadtC ns <$> traverseOf (traverse . _3) (typeVarsEx s f) argTys
                <*> typeVarsEx s f retTy

instance HasTypeVars t => HasTypeVars [t] where
  typeVarsEx s = traverse . typeVarsEx s

instance HasTypeVars t => HasTypeVars (Maybe t) where
  typeVarsEx s = traverse . typeVarsEx s

-- | Traverse /free/ type variables
typeVars :: HasTypeVars t => Traversal' t Name
typeVars = typeVarsEx mempty

-- | Substitute using a map of names in for /free/ type variables
substTypeVars :: HasTypeVars t => Map Name Name -> t -> t
substTypeVars m = over typeVars $ \n -> fromMaybe n (m^.at n)

-- | Provides substitution for types
class SubstType t where
  -- | Perform substitution for types
  substType :: Map Name Type -> t -> t

instance SubstType Type where
  substType m t@(VarT n)           = fromMaybe t (m^.at n)
  substType m (ForallT bs ctx ty)  = ForallT bs (substType m' ctx) (substType m' ty)
    where m' = foldrOf typeVars Map.delete m bs
  substType _ t@ConT{}             = t
  substType _ t@TupleT{}           = t
  substType _ t@ListT{}            = t
  substType _ t@ArrowT{}           = t
  substType _ t@UnboxedTupleT{}    = t
  substType m (AppT l r)           = AppT (substType m l) (substType m r)
  substType m (SigT t k)           = SigT (substType m t)
                                          (substType m k)
  substType _ t@PromotedT{}        = t
  substType _ t@PromotedTupleT{}   = t
  substType _ t@PromotedNilT{}     = t
  substType _ t@PromotedConsT{}    = t
  substType _ t@StarT{}            = t
  substType _ t@ConstraintT{}      = t
  substType _ t@LitT{}             = t
  substType _ t@EqualityT{}        = t
  substType m (InfixT  t1 n t2)    = InfixT  (substType m t1) n (substType m t2)
  substType m (UInfixT t1 n t2)    = UInfixT (substType m t1) n (substType m t2)
  substType m (ParensT t)          = ParensT (substType m t)
  substType _ t@WildCardT{}        = t
#if MIN_VERSION_template_haskell(2,12,0)
  substType _ t@UnboxedSumT{}      = t
#endif
#if MIN_VERSION_template_haskell(2,15,0)
  substType m (AppKindT t k)       = AppKindT (substType m t) (substType m k)
  substType m (ImplicitParamT n t) = ImplicitParamT n (substType m t)
#endif
#if MIN_VERSION_template_haskell(2,16,0)
  substType m (ForallVisT bs ty)   = ForallVisT bs (substType m' ty)
    where m' = foldrOf typeVars Map.delete m bs
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  substType _ t@MulArrowT{}        = t
#endif

instance SubstType t => SubstType [t] where
  substType = map . substType

-- | Provides a 'Traversal' of the types of each field of a constructor.
conFields :: Traversal' Con BangType
conFields f (NormalC n fs)      = NormalC n <$> traverse f fs
conFields f (RecC n fs)         = RecC n <$> traverse (sansVar f) fs
conFields f (InfixC l n r)      = InfixC <$> f l <*> pure n <*> f r
conFields f (ForallC bds ctx c) = ForallC bds ctx <$> conFields f c
conFields f (GadtC ns argTys retTy) =
  GadtC ns <$> traverse f argTys <*> pure retTy
conFields f (RecGadtC ns argTys retTy) =
  RecGadtC ns <$> traverse (sansVar f) argTys <*> pure retTy

sansVar :: Traversal' VarBangType BangType
sansVar f (fn,s,t) = (\(s', t') -> (fn,s',t')) <$> f (s, t)

-- | 'Traversal' of the types of the /named/ fields of a constructor.
conNamedFields :: Traversal' Con VarBangType
conNamedFields _ c@NormalC{}      = pure c
conNamedFields _ c@InfixC{}       = pure c
conNamedFields f (RecC n fs)      = RecC n <$> traverse f fs
conNamedFields f (ForallC a b fs) = ForallC a b <$> conNamedFields f fs
conNamedFields _ c@GadtC{}        = pure c
conNamedFields f (RecGadtC ns argTys retTy) =
  RecGadtC ns <$> traverse f argTys <*> pure retTy

-- Lenses and Prisms
locFileName :: Lens' Loc String
locFileName = lens loc_filename
            $ \loc fn -> loc { loc_filename = fn }

locPackage :: Lens' Loc String
locPackage = lens loc_package
           $ \loc fn -> loc { loc_package = fn }

locModule :: Lens' Loc String
locModule = lens loc_module
          $ \loc fn -> loc { loc_module = fn }

locStart :: Lens' Loc CharPos
locStart = lens loc_start
         $ \loc fn -> loc { loc_start = fn }

locEnd :: Lens' Loc CharPos
locEnd = lens loc_end
       $ \loc fn -> loc { loc_end = fn }

funDepInputs :: Lens' FunDep [Name]
funDepInputs = lens g s where
   g (FunDep xs _)    = xs
   s (FunDep _ ys) xs = FunDep xs ys

funDepOutputs :: Lens' FunDep [Name]
funDepOutputs = lens g s where
   g (FunDep _ xs) = xs
   s (FunDep ys _) = FunDep ys

fieldExpName :: Lens' FieldExp Name
fieldExpName = _1

fieldExpExpression :: Lens' FieldExp Exp
fieldExpExpression = _2

fieldPatName :: Lens' FieldPat Name
fieldPatName = _1

fieldPatPattern :: Lens' FieldPat Pat
fieldPatPattern = _2

matchPattern :: Lens' Match Pat
matchPattern = lens g s where
   g (Match p _ _)   = p
   s (Match _ x y) p = Match p x y

matchBody :: Lens' Match Body
matchBody = lens g s where
   g (Match _ b _)   = b
   s (Match x _ y) b = Match x b y

matchDeclarations :: Lens' Match [Dec]
matchDeclarations = lens g s where
   g (Match _ _ ds) = ds
   s (Match x y _ ) = Match x y

fixityPrecedence :: Lens' Fixity Int
fixityPrecedence = lens g s where
   g (Fixity i _)   = i
   s (Fixity _ x) i = Fixity i x

fixityDirection :: Lens' Fixity FixityDirection
fixityDirection = lens g s where
   g (Fixity _ d) = d
   s (Fixity i _) = Fixity i

clausePattern :: Lens' Clause [Pat]
clausePattern = lens g s where
   g (Clause ps _ _)    = ps
   s (Clause _  x y) ps = Clause ps x y

clauseBody :: Lens' Clause Body
clauseBody = lens g s where
   g (Clause _ b _)   = b
   s (Clause x _ y) b = Clause x b y

clauseDecs :: Lens' Clause [Dec]
clauseDecs = lens g s where
   g (Clause _ _ ds) = ds
   s (Clause x y _ ) = Clause x y

injectivityAnnOutput :: Lens' InjectivityAnn Name
injectivityAnnOutput = lens g s where
   g (InjectivityAnn o _)   = o
   s (InjectivityAnn _ i) o = InjectivityAnn o i

injectivityAnnInputs :: Lens' InjectivityAnn [Name]
injectivityAnnInputs = lens g s where
   g (InjectivityAnn _ i) = i
   s (InjectivityAnn o _) = InjectivityAnn o

typeFamilyHeadName :: Lens' TypeFamilyHead Name
typeFamilyHeadName = lens g s where
  g (TypeFamilyHead n _    _  _ )   = n
  s (TypeFamilyHead _ tvbs rs ia) n = TypeFamilyHead n tvbs rs ia

typeFamilyHeadTyVarBndrs :: Lens' TypeFamilyHead [TyVarBndrUnit]
typeFamilyHeadTyVarBndrs = lens g s where
  g (TypeFamilyHead _ tvbs _  _ )      = tvbs
  s (TypeFamilyHead n _    rs ia) tvbs = TypeFamilyHead n tvbs rs ia

typeFamilyHeadResultSig :: Lens' TypeFamilyHead FamilyResultSig
typeFamilyHeadResultSig = lens g s where
  g (TypeFamilyHead _ _    rs _ )    = rs
  s (TypeFamilyHead n tvbs _  ia) rs = TypeFamilyHead n tvbs rs ia

typeFamilyHeadInjectivityAnn :: Lens' TypeFamilyHead (Maybe InjectivityAnn)
typeFamilyHeadInjectivityAnn = lens g s where
  g (TypeFamilyHead _ _    _  ia) = ia
  s (TypeFamilyHead n tvbs rs _ ) = TypeFamilyHead n tvbs rs

bangSourceUnpackedness :: Lens' Bang SourceUnpackedness
bangSourceUnpackedness = lens g s where
  g (Bang su _ )    = su
  s (Bang _  ss) su = Bang su ss

bangSourceStrictness :: Lens' Bang SourceStrictness
bangSourceStrictness = lens g s where
  g (Bang _  su) = su
  s (Bang ss _ ) = Bang ss

#if MIN_VERSION_template_haskell(2,12,0)
derivClauseStrategy :: Lens' DerivClause (Maybe DerivStrategy)
derivClauseStrategy = lens g s where
  g (DerivClause mds _)     = mds
  s (DerivClause _   c) mds = DerivClause mds c

derivClauseCxt :: Lens' DerivClause Cxt
derivClauseCxt = lens g s where
  g (DerivClause _   c) = c
  s (DerivClause mds _) = DerivClause mds
#endif

_ClassI :: Prism' Info (Dec, [InstanceDec])
_ClassI
  = prism' reviewer remitter
  where
      reviewer (x, y) = ClassI x y
      remitter (ClassI x y) = Just (x, y)
      remitter _ = Nothing

_ClassOpI :: Prism' Info (Name, Type, ParentName)
_ClassOpI
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ClassOpI x y z
      remitter (ClassOpI x y z) = Just (x, y, z)
      remitter _ = Nothing

_TyConI :: Prism' Info Dec
_TyConI
  = prism' reviewer remitter
  where
      reviewer = TyConI
      remitter (TyConI x) = Just x
      remitter _ = Nothing

_FamilyI :: Prism' Info (Dec, [InstanceDec])
_FamilyI
  = prism' reviewer remitter
  where
      reviewer (x, y) = FamilyI x y
      remitter (FamilyI x y) = Just (x, y)
      remitter _ = Nothing

_PrimTyConI :: Prism' Info (Name, Arity, Unlifted)
_PrimTyConI
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = PrimTyConI x y z
      remitter (PrimTyConI x y z) = Just (x, y, z)
      remitter _ = Nothing

_DataConI :: Prism' Info (Name, Type, ParentName)
_DataConI
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = DataConI x y z
      remitter (DataConI x y z) = Just (x, y, z)
      remitter _ = Nothing

_VarI :: Prism' Info (Name, Type, Maybe Dec)
_VarI
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = VarI x y z
      remitter (VarI x y z) = Just (x, y, z)
      remitter _ = Nothing

_TyVarI :: Prism' Info (Name, Type)
_TyVarI
  = prism' reviewer remitter
  where
      reviewer (x, y) = TyVarI x y
      remitter (TyVarI x y) = Just (x, y)
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_PatSynI :: Prism' Info (Name, PatSynType)
_PatSynI
  = prism' reviewer remitter
  where
      reviewer (x, y) = PatSynI x y
      remitter (PatSynI x y) = Just (x, y)
      remitter _ = Nothing
#endif

_FunD :: Prism' Dec (Name, [Clause])
_FunD
  = prism' reviewer remitter
  where
      reviewer (x, y) = FunD x y
      remitter (FunD x y) = Just (x,y)
      remitter _ = Nothing

_ValD :: Prism' Dec (Pat, Body, [Dec])
_ValD
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ValD x y z
      remitter (ValD x y z) = Just (x, y, z)
      remitter _ = Nothing

_TySynD :: Prism' Dec (Name, [TyVarBndrUnit], Type)
_TySynD
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = TySynD x y z
      remitter (TySynD x y z) = Just (x, y, z)
      remitter _ = Nothing

_ClassD :: Prism' Dec (Cxt, Name, [TyVarBndrUnit], [FunDep], [Dec])
_ClassD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u) = ClassD x y z w u
      remitter (ClassD x y z w u) = Just (x, y, z, w, u)
      remitter _ = Nothing

_InstanceD :: Prism' Dec (Maybe Overlap, Cxt, Type, [Dec])
_InstanceD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w) = InstanceD x y z w
      remitter (InstanceD x y z w) = Just (x, y, z, w)
      remitter _ = Nothing

_Overlappable  :: Prism' Overlap  ()
_Overlappable  = prism' reviewer remitter
  where
      reviewer () = Overlappable
      remitter Overlappable = Just  ()
      remitter _ = Nothing

_Overlapping :: Prism' Overlap ()
_Overlapping = prism' reviewer remitter
  where
      reviewer () = Overlapping
      remitter Overlapping = Just ()
      remitter _ = Nothing

_Overlaps ::  Prism' Overlap  ()
_Overlaps =  prism' reviewer remitter
  where
      reviewer () =  Overlaps
      remitter Overlaps = Just ()
      remitter _ = Nothing

_Incoherent  :: Prism' Overlap ()
_Incoherent  = prism' reviewer remitter
  where
      reviewer () = Incoherent
      remitter Incoherent = Just ()
      remitter _ = Nothing

_SigD :: Prism' Dec (Name, Type)
_SigD
  = prism' reviewer remitter
  where
      reviewer (x, y) = SigD x y
      remitter (SigD x y) = Just (x, y)
      remitter _ = Nothing

_ForeignD :: Prism' Dec Foreign
_ForeignD
  = prism' reviewer remitter
  where
      reviewer = ForeignD
      remitter (ForeignD x) = Just x
      remitter _ = Nothing

_InfixD :: Prism' Dec (Fixity, Name)
_InfixD
  = prism' reviewer remitter
  where
      reviewer (x, y) = InfixD x y
      remitter (InfixD x y) = Just (x, y)
      remitter _ = Nothing

_PragmaD :: Prism' Dec Pragma
_PragmaD
  = prism' reviewer remitter
  where
      reviewer = PragmaD
      remitter (PragmaD x) = Just x
      remitter _ = Nothing

-- |
-- @
-- _TySynInstD :: 'Prism'' 'Dec' 'TySynEqn'             -- template-haskell-2.15+
-- _TySynInstD :: 'Prism'' 'Dec' ('Name', 'TySynEqn')     -- template-haskell-2.9 through 2.14
-- _TySynInstD :: 'Prism'' 'Dec' ('Name', ['Type'], 'Type') -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,15,0)
_TySynInstD :: Prism' Dec TySynEqn
_TySynInstD
  = prism' reviewer remitter
  where
      reviewer = TySynInstD
      remitter (TySynInstD x) = Just x
      remitter _ = Nothing
#else
_TySynInstD :: Prism' Dec (Name, TySynEqn)
_TySynInstD
  = prism' reviewer remitter
  where
      reviewer (x, y) = TySynInstD x y
      remitter (TySynInstD x y) = Just (x, y)
      remitter _ = Nothing
#endif

_RoleAnnotD :: Prism' Dec (Name, [Role])
_RoleAnnotD
  = prism' reviewer remitter
  where
      reviewer (x, y) = RoleAnnotD x y
      remitter (RoleAnnotD x y) = Just (x, y)
      remitter _ = Nothing

-- |
-- @
-- _StandaloneDerivD :: 'Prism'' 'Dec' ('Maybe' 'DerivStrategy', 'Cxt', 'Type') -- template-haskell-2.12+
-- _StandaloneDerivD :: 'Prism'' 'Dec'                      ('Cxt', 'Type') -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,12,0)
_StandaloneDerivD :: Prism' Dec (Maybe DerivStrategy, Cxt, Type)
_StandaloneDerivD
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = StandaloneDerivD x y z
      remitter (StandaloneDerivD x y z) = Just (x, y, z)
      remitter _ = Nothing
#else
_StandaloneDerivD :: Prism' Dec (Cxt, Type)
_StandaloneDerivD
  = prism' reviewer remitter
  where
      reviewer (x, y) = StandaloneDerivD x y
      remitter (StandaloneDerivD x y) = Just (x, y)
      remitter _ = Nothing
#endif

_DefaultSigD :: Prism' Dec (Name, Type)
_DefaultSigD
  = prism' reviewer remitter
  where
      reviewer (x, y) = DefaultSigD x y
      remitter (DefaultSigD x y) = Just (x, y)
      remitter _ = Nothing

# if MIN_VERSION_template_haskell(2,12,0)
type DataPrism' tys cons = Prism' Dec (Cxt, Name, tys, Maybe Kind, cons, [DerivClause])
# else
type DataPrism' tys cons = Prism' Dec (Cxt, Name, tys, Maybe Kind, cons, Cxt)
# endif

-- |
-- @
-- _DataInstD :: 'Prism'' 'Dec' ('Cxt', 'Maybe' ['TyVarBndrUnit'], 'Type', 'Maybe' 'Kind', ['Con'], ['DerivClause']) -- template-haskell-2.15+
-- _DataInstD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],                'Maybe' 'Kind', ['Con'], ['DerivClause']) -- template-haskell-2.12 through 2.14
-- _DataInstD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],                'Maybe' 'Kind', ['Con'], 'Cxt')           -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,15,0)
_DataInstD :: Prism' Dec (Cxt, Maybe [TyVarBndrUnit], Type, Maybe Kind, [Con], [DerivClause])
_DataInstD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = DataInstD x y z w u v
      remitter (DataInstD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing
#else
_DataInstD :: DataPrism' [Type] [Con]
_DataInstD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = DataInstD x y z w u v
      remitter (DataInstD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing
#endif

-- |
-- @
-- _NewtypeInstD :: 'Prism'' 'Dec' ('Cxt', 'Maybe' ['TyVarBndrUnit'], 'Type', 'Maybe' 'Kind', 'Con', ['DerivClause']) -- template-haskell-2.15+
-- _NewtypeInstD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],                'Maybe' 'Kind', 'Con', ['DerivClause']) -- template-haskell-2.12 through 2.14
-- _NewtypeInstD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],                'Maybe' 'Kind', 'Con', 'Cxt')           -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,15,0)
_NewtypeInstD :: Prism' Dec (Cxt, Maybe [TyVarBndrUnit], Type, Maybe Kind, Con, [DerivClause])
_NewtypeInstD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = NewtypeInstD x y z w u v
      remitter (NewtypeInstD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing
#else
_NewtypeInstD :: DataPrism' [Type] Con
_NewtypeInstD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = NewtypeInstD x y z w u v
      remitter (NewtypeInstD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing
#endif

_ClosedTypeFamilyD :: Prism' Dec (TypeFamilyHead, [TySynEqn])
_ClosedTypeFamilyD
  = prism' reviewer remitter
  where
      reviewer (x, y) = ClosedTypeFamilyD x y
      remitter (ClosedTypeFamilyD x y) = Just (x, y)
      remitter _ = Nothing

-- |
-- @
-- _DataD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['TyVarBndrUnit'], 'Maybe' 'Kind', ['Con'], ['DerivClause']) -- template-haskell-2.12+
-- _DataD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],          'Maybe' 'Kind', ['Con'], 'Cxt')           -- Earlier versions
-- @
_DataD :: DataPrism' [TyVarBndrUnit] [Con]
_DataD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = DataD x y z w u v
      remitter (DataD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing

-- |
-- @
-- _NewtypeD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['TyVarBndrUnit'], 'Maybe' 'Kind', 'Con', ['DerivClause']) -- template-haskell-2.12+
-- _NewtypeD :: 'Prism'' 'Dec' ('Cxt', 'Name', ['Type'],          'Maybe' 'Kind', 'Con', 'Cxt')           -- Earlier versions
-- @
_NewtypeD :: DataPrism' [TyVarBndrUnit] Con
_NewtypeD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = NewtypeD x y z w u v
      remitter (NewtypeD x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing

_DataFamilyD :: Prism' Dec (Name, [TyVarBndrUnit], Maybe Kind)
_DataFamilyD
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = DataFamilyD x y z
      remitter (DataFamilyD x y z) = Just (x, y, z)
      remitter _ = Nothing

_OpenTypeFamilyD :: Prism' Dec TypeFamilyHead
_OpenTypeFamilyD
  = prism' reviewer remitter
  where
      reviewer = OpenTypeFamilyD
      remitter (OpenTypeFamilyD x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_PatSynD :: Prism' Dec (Name, PatSynArgs, PatSynDir, Pat)
_PatSynD
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w) = PatSynD x y z w
      remitter (PatSynD x y z w) = Just (x, y, z, w)
      remitter _ = Nothing

_PatSynSigD :: Prism' Dec (Name, PatSynType)
_PatSynSigD
  = prism' reviewer remitter
  where
      reviewer (x, y) = PatSynSigD x y
      remitter (PatSynSigD x y) = Just (x, y)
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,15,0)
_ImplicitParamBindD :: Prism' Dec (String, Exp)
_ImplicitParamBindD
  = prism' reviewer remitter
  where
      reviewer (x, y) = ImplicitParamBindD x y
      remitter (ImplicitParamBindD x y) = Just (x, y)
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,12,0)
_Unidir :: Prism' PatSynDir ()
_Unidir
  = prism' reviewer remitter
  where
      reviewer () = Unidir
      remitter Unidir = Just ()
      remitter _ = Nothing

_ImplBidir :: Prism' PatSynDir ()
_ImplBidir
  = prism' reviewer remitter
  where
      reviewer () = ImplBidir
      remitter ImplBidir = Just ()
      remitter _ = Nothing

_ExplBidir :: Prism' PatSynDir [Clause]
_ExplBidir
  = prism' reviewer remitter
  where
      reviewer = ExplBidir
      remitter (ExplBidir x) = Just x
      remitter _ = Nothing

_PrefixPatSyn :: Prism' PatSynArgs [Name]
_PrefixPatSyn
  = prism' reviewer remitter
  where
      reviewer = PrefixPatSyn
      remitter (PrefixPatSyn x) = Just x
      remitter _ = Nothing

_InfixPatSyn :: Prism' PatSynArgs (Name, Name)
_InfixPatSyn
  = prism' reviewer remitter
  where
      reviewer (x, y) = InfixPatSyn x y
      remitter (InfixPatSyn x y) = Just (x, y)
      remitter _ = Nothing

_RecordPatSyn :: Prism' PatSynArgs [Name]
_RecordPatSyn
  = prism' reviewer remitter
  where
      reviewer = RecordPatSyn
      remitter (RecordPatSyn x) = Just x
      remitter _ = Nothing
#endif

_NormalC :: Prism' Con (Name, [BangType])
_NormalC
  = prism' reviewer remitter
  where
      reviewer (x, y) = NormalC x y
      remitter (NormalC x y) = Just (x, y)
      remitter _ = Nothing

_RecC :: Prism' Con (Name, [VarBangType])
_RecC
  = prism' reviewer remitter
  where
      reviewer (x, y) = RecC x y
      remitter (RecC x y) = Just (x, y)
      remitter _ = Nothing

_InfixC :: Prism' Con (BangType, Name, BangType  )
_InfixC
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = InfixC x y z
      remitter (InfixC x y z) = Just (x, y, z)
      remitter _ = Nothing

_ForallC :: Prism' Con ([TyVarBndrSpec], Cxt, Con)
_ForallC
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ForallC x y z
      remitter (ForallC x y z) = Just (x, y, z)
      remitter _ = Nothing

_GadtC :: Prism' Con ([Name], [BangType], Type)
_GadtC
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = GadtC x y z
      remitter (GadtC x y z) = Just (x, y, z)
      remitter _ = Nothing

_RecGadtC :: Prism' Con ([Name], [VarBangType], Type)
_RecGadtC
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = RecGadtC x y z
      remitter (RecGadtC x y z) = Just (x, y, z)
      remitter _ = Nothing

_NoSourceUnpackedness :: Prism' SourceUnpackedness ()
_NoSourceUnpackedness
  = prism' reviewer remitter
  where
      reviewer () = NoSourceUnpackedness
      remitter NoSourceUnpackedness = Just ()
      remitter _ = Nothing

_SourceNoUnpack :: Prism' SourceUnpackedness ()
_SourceNoUnpack
  = prism' reviewer remitter
  where
      reviewer () = SourceNoUnpack
      remitter SourceNoUnpack = Just ()
      remitter _ = Nothing

_SourceUnpack :: Prism' SourceUnpackedness ()
_SourceUnpack
  = prism' reviewer remitter
  where
      reviewer () = SourceUnpack
      remitter SourceUnpack = Just ()
      remitter _ = Nothing

_NoSourceStrictness :: Prism' SourceStrictness ()
_NoSourceStrictness
  = prism' reviewer remitter
  where
      reviewer () = NoSourceStrictness
      remitter NoSourceStrictness = Just ()
      remitter _ = Nothing

_SourceLazy :: Prism' SourceStrictness ()
_SourceLazy
  = prism' reviewer remitter
  where
      reviewer () = SourceLazy
      remitter SourceLazy = Just ()
      remitter _ = Nothing

_SourceStrict :: Prism' SourceStrictness ()
_SourceStrict
  = prism' reviewer remitter
  where
      reviewer () = SourceStrict
      remitter SourceStrict = Just ()
      remitter _ = Nothing

_DecidedLazy :: Prism' DecidedStrictness ()
_DecidedLazy
  = prism' reviewer remitter
  where
      reviewer () = DecidedLazy
      remitter DecidedLazy = Just ()
      remitter _ = Nothing

_DecidedStrict :: Prism' DecidedStrictness ()
_DecidedStrict
  = prism' reviewer remitter
  where
      reviewer () = DecidedStrict
      remitter DecidedStrict = Just ()
      remitter _ = Nothing

_DecidedUnpack :: Prism' DecidedStrictness ()
_DecidedUnpack
  = prism' reviewer remitter
  where
      reviewer () = DecidedUnpack
      remitter DecidedUnpack = Just ()
      remitter _ = Nothing

_ImportF :: Prism' Foreign (Callconv, Safety, String, Name, Type)
_ImportF
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u) = ImportF x y z w u
      remitter (ImportF x y z w u) = Just (x,y,z,w,u)
      remitter _ = Nothing

_ExportF :: Prism' Foreign (Callconv, String, Name, Type)
_ExportF
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w) = ExportF x y z w
      remitter (ExportF x y z w) = Just (x, y, z, w)
      remitter _ = Nothing

_CCall :: Prism' Callconv ()
_CCall
  = prism' reviewer remitter
  where
      reviewer () = CCall
      remitter CCall = Just ()
      remitter _ = Nothing

_StdCall :: Prism' Callconv ()
_StdCall
  = prism' reviewer remitter
  where
      reviewer () = StdCall
      remitter StdCall = Just ()
      remitter _ = Nothing

_CApi :: Prism' Callconv ()
_CApi
  = prism' reviewer remitter
  where
      reviewer () = CApi
      remitter CApi = Just ()
      remitter _ = Nothing

_Prim :: Prism' Callconv ()
_Prim
  = prism' reviewer remitter
  where
      reviewer () = Prim
      remitter Prim = Just ()
      remitter _ = Nothing

_JavaScript :: Prism' Callconv ()
_JavaScript
  = prism' reviewer remitter
  where
      reviewer () = JavaScript
      remitter JavaScript = Just ()
      remitter _ = Nothing

_Unsafe :: Prism' Safety ()
_Unsafe
  = prism' reviewer remitter
  where
      reviewer () = Unsafe
      remitter Unsafe = Just ()
      remitter _ = Nothing

_Safe :: Prism' Safety ()
_Safe
  = prism' reviewer remitter
  where
      reviewer () = Safe
      remitter Safe = Just ()
      remitter _ = Nothing

_Interruptible :: Prism' Safety ()
_Interruptible
  = prism' reviewer remitter
  where
      reviewer () = Interruptible
      remitter Interruptible = Just ()
      remitter _ = Nothing

_InlineP :: Prism' Pragma (Name, Inline, RuleMatch, Phases)
_InlineP
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w) = InlineP x y z w
      remitter (InlineP x y z w) = Just (x, y, z, w)
      remitter _ = Nothing

_SpecialiseP :: Prism' Pragma (Name, Type, Maybe Inline, Phases)
_SpecialiseP
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w) = SpecialiseP x y z w
      remitter (SpecialiseP x y z w) = Just (x, y, z, w)
      remitter _ = Nothing

-- TODO add lenses for InlineSpec

_SpecialiseInstP :: Prism' Pragma Type
_SpecialiseInstP
  = prism' reviewer remitter
  where
      reviewer = SpecialiseInstP
      remitter (SpecialiseInstP x) = Just x
      remitter _ = Nothing

-- |
-- @
-- _RuleP :: 'Prism'' 'Pragma' ('String', 'Maybe' ['TyVarBndrUnit'], ['RuleBndr'], 'Exp', 'Exp', 'Phases') -- template-haskell-2.15+
-- _RuleP :: 'Prism'' 'Pragma' ('String',                        ['RuleBndr'], 'Exp', 'Exp', 'Phases') -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,15,0)
_RuleP :: Prism' Pragma (String, Maybe [TyVarBndrUnit], [RuleBndr], Exp, Exp, Phases)
_RuleP
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u, v) = RuleP x y z w u v
      remitter (RuleP x y z w u v) = Just (x, y, z, w, u, v)
      remitter _ = Nothing
#else
_RuleP :: Prism' Pragma (String, [RuleBndr], Exp, Exp, Phases)
_RuleP
  = prism' reviewer remitter
  where
      reviewer (x, y, z, w, u) = RuleP x y z w u
      remitter (RuleP x y z w u) = Just (x, y, z, w, u)
      remitter _ = Nothing
#endif

_AnnP :: Prism' Pragma (AnnTarget, Exp)
_AnnP
  = prism' reviewer remitter
  where
      reviewer (x, y) = AnnP x y
      remitter (AnnP x y) = Just (x, y)
      remitter _ = Nothing

_LineP :: Prism' Pragma (Int, String)
_LineP
  = prism' reviewer remitter
  where
      reviewer (x, y) = LineP x y
      remitter (LineP x y) = Just (x, y)
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_CompleteP :: Prism' Pragma ([Name], Maybe Name)
_CompleteP
  = prism' reviewer remitter
  where
      reviewer (x, y) = CompleteP x y
      remitter (CompleteP x y) = Just (x, y)
      remitter _ = Nothing
#endif

_NoInline :: Prism' Inline ()
_NoInline
  = prism' reviewer remitter
  where
      reviewer () = NoInline
      remitter NoInline = Just ()
      remitter _ = Nothing

_Inline :: Prism' Inline ()
_Inline
  = prism' reviewer remitter
  where
      reviewer () = Inline
      remitter Inline = Just ()
      remitter _ = Nothing

_Inlinable :: Prism' Inline ()
_Inlinable
  = prism' reviewer remitter
  where
      reviewer () = Inlinable
      remitter Inlinable = Just ()
      remitter _ = Nothing

_ConLike :: Prism' RuleMatch ()
_ConLike
  = prism' reviewer remitter
  where
      reviewer () = ConLike
      remitter ConLike = Just ()
      remitter _ = Nothing

_FunLike :: Prism' RuleMatch ()
_FunLike
  = prism' reviewer remitter
  where
      reviewer () = FunLike
      remitter FunLike = Just ()
      remitter _ = Nothing

_AllPhases :: Prism' Phases ()
_AllPhases
  = prism' reviewer remitter
  where
      reviewer () = AllPhases
      remitter AllPhases = Just ()
      remitter _ = Nothing

_FromPhase :: Prism' Phases Int
_FromPhase
  = prism' reviewer remitter
  where
      reviewer = FromPhase
      remitter (FromPhase x) = Just x
      remitter _ = Nothing

_BeforePhase :: Prism' Phases Int
_BeforePhase
  = prism' reviewer remitter
  where
      reviewer = BeforePhase
      remitter (BeforePhase x) = Just x
      remitter _ = Nothing

_RuleVar :: Prism' RuleBndr Name
_RuleVar
  = prism' reviewer remitter
  where
      reviewer = RuleVar
      remitter (RuleVar x) = Just x
      remitter _ = Nothing

_TypedRuleVar :: Prism' RuleBndr (Name, Type)
_TypedRuleVar
  = prism' reviewer remitter
  where
      reviewer (x, y) = TypedRuleVar x y
      remitter (TypedRuleVar x y) = Just (x, y)
      remitter _ = Nothing

_ModuleAnnotation :: Prism' AnnTarget ()
_ModuleAnnotation
  = prism' reviewer remitter
  where
      reviewer () = ModuleAnnotation
      remitter ModuleAnnotation = Just ()
      remitter _ = Nothing

_TypeAnnotation :: Prism' AnnTarget Name
_TypeAnnotation
  = prism' reviewer remitter
  where
      reviewer = TypeAnnotation
      remitter (TypeAnnotation x) = Just x
      remitter _ = Nothing

_ValueAnnotation :: Prism' AnnTarget Name
_ValueAnnotation
  = prism' reviewer remitter
  where
      reviewer = ValueAnnotation
      remitter (ValueAnnotation x) = Just x
      remitter _ = Nothing

_FunDep :: Iso' FunDep ([Name], [Name])
_FunDep
  = iso remitter reviewer
  where
      reviewer (x, y) = FunDep x y
      remitter (FunDep x y) = (x, y)

#if !(MIN_VERSION_template_haskell(2,13,0))
_TypeFam :: Prism' FamFlavour ()
_TypeFam
  = prism' reviewer remitter
  where
      reviewer () = TypeFam
      remitter TypeFam = Just ()
      remitter _ = Nothing

_DataFam :: Prism' FamFlavour ()
_DataFam
  = prism' reviewer remitter
  where
      reviewer () = DataFam
      remitter DataFam = Just ()
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,15,0)
tySynEqnLHS :: Lens' TySynEqn Type
tySynEqnLHS = lens g s where
   g (TySynEqn _     lhs _)       = lhs
   s (TySynEqn mtvbs _   rhs) lhs = TySynEqn mtvbs lhs rhs

tySynEqnPatterns :: Lens' TySynEqn [Type]
tySynEqnPatterns = lens g s where
   g (TySynEqn _     lhs _) = pats
     where (_n, pats) = unfoldType lhs
   s (TySynEqn mtvbs lhs rhs) pats = TySynEqn mtvbs (F.foldl' AppT n pats) rhs
     where (n, _pats) = unfoldType lhs

tySynEqnResult :: Lens' TySynEqn Type
tySynEqnResult = lens g s where
   g (TySynEqn _     _   rhs) = rhs
   s (TySynEqn mtvbs lhs _)   = TySynEqn mtvbs lhs
#else
tySynEqnPatterns :: Lens' TySynEqn [Type]
tySynEqnPatterns = lens g s where
   g (TySynEqn xs _)    = xs
   s (TySynEqn _  y) xs = TySynEqn xs y

tySynEqnResult :: Lens' TySynEqn Type
tySynEqnResult = lens g s where
   g (TySynEqn _  x) = x
   s (TySynEqn xs _) = TySynEqn xs
#endif

_InfixL :: Prism' FixityDirection ()
_InfixL
  = prism' reviewer remitter
  where
      reviewer () = InfixL
      remitter InfixL = Just ()
      remitter _ = Nothing

_InfixR :: Prism' FixityDirection ()
_InfixR
  = prism' reviewer remitter
  where
      reviewer () = InfixR
      remitter InfixR = Just ()
      remitter _ = Nothing

_InfixN :: Prism' FixityDirection ()
_InfixN
  = prism' reviewer remitter
  where
      reviewer () = InfixN
      remitter InfixN = Just ()
      remitter _ = Nothing

_VarE :: Prism' Exp Name
_VarE
  = prism' reviewer remitter
  where
      reviewer = VarE
      remitter (VarE x) = Just x
      remitter _ = Nothing

_ConE :: Prism' Exp Name
_ConE
  = prism' reviewer remitter
  where
      reviewer = ConE
      remitter (ConE x) = Just x
      remitter _ = Nothing

_LitE :: Prism' Exp Lit
_LitE
  = prism' reviewer remitter
  where
      reviewer = LitE
      remitter (LitE x) = Just x
      remitter _ = Nothing

_AppE :: Prism' Exp (Exp, Exp)
_AppE
  = prism' reviewer remitter
  where
      reviewer (x, y) = AppE x y
      remitter (AppE x y) = Just (x, y)
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_AppTypeE :: Prism' Exp (Exp, Type)
_AppTypeE
  = prism' reviewer remitter
  where
      reviewer (x, y) = AppTypeE x y
      remitter (AppTypeE x y) = Just (x, y)
      remitter _ = Nothing
#endif

_InfixE :: Prism' Exp (Maybe Exp, Exp, Maybe Exp)
_InfixE
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = InfixE x y z
      remitter (InfixE x y z) = Just (x, y, z)
      remitter _ = Nothing

_UInfixE :: Prism' Exp (Exp, Exp, Exp)
_UInfixE
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = UInfixE x y z
      remitter (UInfixE x y z) = Just (x, y, z)
      remitter _ = Nothing

_ParensE :: Prism' Exp Exp
_ParensE
  = prism' reviewer remitter
  where
      reviewer = ParensE
      remitter (ParensE x) = Just x
      remitter _ = Nothing

_LamE :: Prism' Exp ([Pat], Exp)
_LamE
  = prism' reviewer remitter
  where
      reviewer (x, y) = LamE x y
      remitter (LamE x y) = Just (x, y)
      remitter _ = Nothing

_LamCaseE :: Prism' Exp [Match]
_LamCaseE
  = prism' reviewer remitter
  where
      reviewer = LamCaseE
      remitter (LamCaseE x) = Just x
      remitter _ = Nothing

-- |
-- @
-- _TupE :: 'Prism'' 'Exp' ['Maybe' 'Exp'] -- template-haskell-2.16+
-- _TupE :: 'Prism'' 'Exp' ['Exp']       -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,16,0)
_TupE :: Prism' Exp [Maybe Exp]
#else
_TupE :: Prism' Exp [Exp]
#endif
_TupE
  = prism' reviewer remitter
  where
      reviewer = TupE
      remitter (TupE x) = Just x
      remitter _ = Nothing

-- |
-- @
-- _UnboxedTupE :: 'Prism'' 'Exp' ['Maybe' 'Exp'] -- template-haskell-2.16+
-- _UnboxedTupE :: 'Prism'' 'Exp' ['Exp']       -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,16,0)
_UnboxedTupE :: Prism' Exp [Maybe Exp]
#else
_UnboxedTupE :: Prism' Exp [Exp]
#endif
_UnboxedTupE
  = prism' reviewer remitter
  where
      reviewer = UnboxedTupE
      remitter (UnboxedTupE x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_UnboxedSumE :: Prism' Exp (Exp, SumAlt, SumArity)
_UnboxedSumE
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = UnboxedSumE x y z
      remitter (UnboxedSumE x y z) = Just (x, y, z)
      remitter _ = Nothing
#endif

_CondE :: Prism' Exp (Exp, Exp, Exp)
_CondE
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = CondE x y z
      remitter (CondE x y z) = Just (x, y, z)
      remitter _ = Nothing

_MultiIfE :: Prism' Exp [(Guard, Exp)]
_MultiIfE
  = prism' reviewer remitter
  where
      reviewer = MultiIfE
      remitter (MultiIfE x) = Just x
      remitter _ = Nothing

_LetE :: Prism' Exp ([Dec], Exp)
_LetE
  = prism' reviewer remitter
  where
      reviewer (x, y) = LetE x y
      remitter (LetE x y) = Just (x, y)
      remitter _ = Nothing

_CaseE :: Prism' Exp (Exp, [Match])
_CaseE
  = prism' reviewer remitter
  where
      reviewer (x, y) = CaseE x y
      remitter (CaseE x y) = Just (x, y)
      remitter _ = Nothing

-- |
-- @
-- _DoE :: 'Prism'' 'Exp' ('Maybe' 'ModName', ['Stmt']) -- template-haskell-2.17+
-- _DoE :: 'Prism'' 'Exp' ['Stmt']                  -- Earlier versions
-- @
# if MIN_VERSION_template_haskell(2,17,0)
_DoE :: Prism' Exp (Maybe ModName, [Stmt])
_DoE
  = prism' reviewer remitter
  where
      reviewer (x, y) = DoE x y
      remitter (DoE x y) = Just (x, y)
      remitter _ = Nothing
# else
_DoE :: Prism' Exp [Stmt]
_DoE
  = prism' reviewer remitter
  where
      reviewer = DoE
      remitter (DoE x) = Just x
      remitter _ = Nothing
# endif

_CompE :: Prism' Exp [Stmt]
_CompE
  = prism' reviewer remitter
  where
      reviewer = CompE
      remitter (CompE x) = Just x
      remitter _ = Nothing

_ArithSeqE :: Prism' Exp Range
_ArithSeqE
  = prism' reviewer remitter
  where
      reviewer = ArithSeqE
      remitter (ArithSeqE x) = Just x
      remitter _ = Nothing

_ListE :: Prism' Exp [Exp]
_ListE
  = prism' reviewer remitter
  where
      reviewer = ListE
      remitter (ListE x) = Just x
      remitter _ = Nothing

_SigE :: Prism' Exp (Exp, Type)
_SigE
  = prism' reviewer remitter
  where
      reviewer (x, y) = SigE x y
      remitter (SigE x y) = Just (x, y)
      remitter _ = Nothing

_RecConE :: Prism' Exp (Name, [FieldExp])
_RecConE
  = prism' reviewer remitter
  where
      reviewer (x, y) = RecConE x y
      remitter (RecConE x y) = Just (x, y)
      remitter _ = Nothing

_RecUpdE :: Prism' Exp (Exp, [FieldExp])
_RecUpdE
  = prism' reviewer remitter
  where
      reviewer (x, y) = RecUpdE x y
      remitter (RecUpdE x y) = Just (x, y)
      remitter _ = Nothing

_StaticE :: Prism' Exp Exp
_StaticE
  = prism' reviewer remitter
  where
      reviewer = StaticE
      remitter (StaticE x) = Just x
      remitter _ = Nothing

_UnboundVarE :: Prism' Exp Name
_UnboundVarE
  = prism' reviewer remitter
  where
      reviewer = UnboundVarE
      remitter (UnboundVarE x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,13,0)
_LabelE :: Prism' Exp String
_LabelE
  = prism' reviewer remitter
  where
      reviewer = LabelE
      remitter (LabelE x) = Just x
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,15,0)
-- |
-- @
-- _MDoE :: 'Prism'' 'Exp' ('Maybe' 'ModName', ['Stmt']) -- template-haskell-2.17+
-- _MDoE :: 'Prism'' 'Exp' ['Stmt']                  -- Earlier versions
-- @
# if MIN_VERSION_template_haskell(2,17,0)
_MDoE :: Prism' Exp (Maybe ModName, [Stmt])
_MDoE
  = prism' reviewer remitter
  where
      reviewer (x, y) = MDoE x y
      remitter (MDoE x y) = Just (x, y)
      remitter _ = Nothing
# else
_MDoE :: Prism' Exp [Stmt]
_MDoE
  = prism' reviewer remitter
  where
      reviewer = MDoE
      remitter (MDoE x) = Just x
      remitter _ = Nothing
# endif

_ImplicitParamVarE :: Prism' Exp String
_ImplicitParamVarE
  = prism' reviewer remitter
  where
      reviewer = ImplicitParamVarE
      remitter (ImplicitParamVarE x) = Just x
      remitter _ = Nothing
#endif

_GuardedB :: Prism' Body [(Guard, Exp)]
_GuardedB
  = prism' reviewer remitter
  where
      reviewer = GuardedB
      remitter (GuardedB x) = Just x
      remitter _ = Nothing

_NormalB :: Prism' Body Exp
_NormalB
  = prism' reviewer remitter
  where
      reviewer = NormalB
      remitter (NormalB x) = Just x
      remitter _ = Nothing

_NormalG :: Prism' Guard Exp
_NormalG
  = prism' reviewer remitter
  where
      reviewer = NormalG
      remitter (NormalG x) = Just x
      remitter _ = Nothing

_PatG :: Prism' Guard [Stmt]
_PatG
  = prism' reviewer remitter
  where
      reviewer = PatG
      remitter (PatG x) = Just x
      remitter _ = Nothing

_BindS :: Prism' Stmt (Pat, Exp)
_BindS
  = prism' reviewer remitter
  where
      reviewer (x, y) = BindS x y
      remitter (BindS x y) = Just (x, y)
      remitter _ = Nothing

_LetS :: Prism' Stmt [Dec]
_LetS
  = prism' reviewer remitter
  where
      reviewer = LetS
      remitter (LetS x) = Just x
      remitter _ = Nothing

_NoBindS :: Prism' Stmt Exp
_NoBindS
  = prism' reviewer remitter
  where
      reviewer = NoBindS
      remitter (NoBindS x) = Just x
      remitter _ = Nothing

_ParS :: Prism' Stmt [[Stmt]]
_ParS
  = prism' reviewer remitter
  where
      reviewer = ParS
      remitter (ParS x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,15,0)
_RecS :: Prism' Stmt [Stmt]
_RecS
  = prism' reviewer remitter
  where
      reviewer = RecS
      remitter (RecS x) = Just x
      remitter _ = Nothing
#endif

_FromR :: Prism' Range Exp
_FromR
  = prism' reviewer remitter
  where
      reviewer = FromR
      remitter (FromR x) = Just x
      remitter _ = Nothing

_FromThenR :: Prism' Range (Exp, Exp)
_FromThenR
  = prism' reviewer remitter
  where
      reviewer (x, y) = FromThenR x y
      remitter (FromThenR x y) = Just (x, y)
      remitter _ = Nothing

_FromToR :: Prism' Range (Exp, Exp)
_FromToR
  = prism' reviewer remitter
  where
      reviewer (x, y) = FromToR x y
      remitter (FromToR x y) = Just (x, y)
      remitter _ = Nothing

_FromThenToR :: Prism' Range (Exp, Exp, Exp)
_FromThenToR
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = FromThenToR x y z
      remitter (FromThenToR x y z) = Just (x, y, z)
      remitter _ = Nothing

_CharL :: Prism' Lit Char
_CharL
  = prism' reviewer remitter
  where
      reviewer = CharL
      remitter (CharL x) = Just x
      remitter _ = Nothing

_StringL :: Prism' Lit String
_StringL
  = prism' reviewer remitter
  where
      reviewer = StringL
      remitter (StringL x) = Just x
      remitter _ = Nothing

_IntegerL :: Prism' Lit Integer
_IntegerL
  = prism' reviewer remitter
  where
      reviewer = IntegerL
      remitter (IntegerL x) = Just x
      remitter _ = Nothing

_RationalL :: Prism' Lit Rational
_RationalL
  = prism' reviewer remitter
  where
      reviewer = RationalL
      remitter (RationalL x) = Just x
      remitter _ = Nothing

_IntPrimL :: Prism' Lit Integer
_IntPrimL
  = prism' reviewer remitter
  where
      reviewer = IntPrimL
      remitter (IntPrimL x) = Just x
      remitter _ = Nothing

_WordPrimL :: Prism' Lit Integer
_WordPrimL
  = prism' reviewer remitter
  where
      reviewer = WordPrimL
      remitter (WordPrimL x) = Just x
      remitter _ = Nothing

_FloatPrimL :: Prism' Lit Rational
_FloatPrimL
  = prism' reviewer remitter
  where
      reviewer = FloatPrimL
      remitter (FloatPrimL x) = Just x
      remitter _ = Nothing

_DoublePrimL :: Prism' Lit Rational
_DoublePrimL
  = prism' reviewer remitter
  where
      reviewer = DoublePrimL
      remitter (DoublePrimL x) = Just x
      remitter _ = Nothing

_StringPrimL :: Prism' Lit [Word8]
_StringPrimL
  = prism' reviewer remitter
  where
      reviewer = StringPrimL
      remitter (StringPrimL x) = Just x
      remitter _ = Nothing

_CharPrimL :: Prism' Lit Char
_CharPrimL
  = prism' reviewer remitter
  where
      reviewer = CharPrimL
      remitter (CharPrimL x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,16,0)
_BytesPrimL :: Prism' Lit Bytes
_BytesPrimL
  = prism' reviewer remitter
  where
      reviewer = BytesPrimL
      remitter (BytesPrimL x) = Just x
      remitter _ = Nothing
#endif

_LitP :: Prism' Pat Lit
_LitP
  = prism' reviewer remitter
  where
      reviewer = LitP
      remitter (LitP x) = Just x
      remitter _ = Nothing

_VarP :: Prism' Pat Name
_VarP
  = prism' reviewer remitter
  where
      reviewer = VarP
      remitter (VarP x) = Just x
      remitter _ = Nothing

_TupP :: Prism' Pat [Pat]
_TupP
  = prism' reviewer remitter
  where
      reviewer = TupP
      remitter (TupP x) = Just x
      remitter _ = Nothing

_UnboxedTupP :: Prism' Pat [Pat]
_UnboxedTupP
  = prism' reviewer remitter
  where
      reviewer = UnboxedTupP
      remitter (UnboxedTupP x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_UnboxedSumP :: Prism' Pat (Pat, SumAlt, SumArity)
_UnboxedSumP
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = UnboxedSumP x y z
      remitter (UnboxedSumP x y z) = Just (x, y, z)
      remitter _ = Nothing
#endif

-- |
-- @
-- _ConP :: 'Prism'' 'Pat' ('Name', ['Type'], 'Pat') -- template-haskell-2.18+
-- _ConP :: 'Prism'' 'Pat' ('Name',         'Pat') -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,18,0)
_ConP :: Prism' Pat (Name, [Type], [Pat])
_ConP
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ConP x y z
      remitter (ConP x y z) = Just (x, y, z)
      remitter _ = Nothing
#else
_ConP :: Prism' Pat (Name, [Pat])
_ConP
  = prism' reviewer remitter
  where
      reviewer (x, y) = ConP x y
      remitter (ConP x y) = Just (x, y)
      remitter _ = Nothing
#endif

_InfixP :: Prism' Pat (Pat, Name, Pat)
_InfixP
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = InfixP x y z
      remitter (InfixP x y z) = Just (x, y, z)
      remitter _ = Nothing

_UInfixP :: Prism' Pat (Pat, Name, Pat)
_UInfixP
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = UInfixP x y z
      remitter (UInfixP x y z) = Just (x, y, z)
      remitter _ = Nothing

_ParensP :: Prism' Pat Pat
_ParensP
  = prism' reviewer remitter
  where
      reviewer = ParensP
      remitter (ParensP x) = Just x
      remitter _ = Nothing

_TildeP :: Prism' Pat Pat
_TildeP
  = prism' reviewer remitter
  where
      reviewer = TildeP
      remitter (TildeP x) = Just x
      remitter _ = Nothing

_BangP :: Prism' Pat Pat
_BangP
  = prism' reviewer remitter
  where
      reviewer = BangP
      remitter (BangP x) = Just x
      remitter _ = Nothing

_AsP :: Prism' Pat (Name, Pat)
_AsP
  = prism' reviewer remitter
  where
      reviewer (x, y) = AsP x y
      remitter (AsP x y) = Just (x, y)
      remitter _ = Nothing

_WildP :: Prism' Pat ()
_WildP
  = prism' reviewer remitter
  where
      reviewer () = WildP
      remitter WildP = Just ()
      remitter _ = Nothing

_RecP :: Prism' Pat (Name, [FieldPat])
_RecP
  = prism' reviewer remitter
  where
      reviewer (x, y) = RecP x y
      remitter (RecP x y) = Just (x, y)
      remitter _ = Nothing

_ListP :: Prism' Pat [Pat]
_ListP
  = prism' reviewer remitter
  where
      reviewer = ListP
      remitter (ListP x) = Just x
      remitter _ = Nothing

_SigP :: Prism' Pat (Pat, Type)
_SigP
  = prism' reviewer remitter
  where
      reviewer (x, y) = SigP x y
      remitter (SigP x y) = Just (x, y)
      remitter _ = Nothing

_ViewP :: Prism' Pat (Exp, Pat)
_ViewP
  = prism' reviewer remitter
  where
      reviewer (x, y) = ViewP x y
      remitter (ViewP x y) = Just (x, y)
      remitter _ = Nothing

_ForallT :: Prism' Type ([TyVarBndrSpec], Cxt, Type)
_ForallT
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ForallT x y z
      remitter (ForallT x y z) = Just (x, y, z)
      remitter _ = Nothing

_AppT :: Prism' Type (Type, Type)
_AppT
  = prism' reviewer remitter
  where
      reviewer (x, y) = AppT x y
      remitter (AppT x y) = Just (x, y)
      remitter _ = Nothing

_SigT :: Prism' Type (Type, Kind)
_SigT
  = prism' reviewer remitter
  where
      reviewer (x, y) = SigT x y
      remitter (SigT x y) = Just (x, y)
      remitter _ = Nothing

_VarT :: Prism' Type Name
_VarT
  = prism' reviewer remitter
  where
      reviewer = VarT
      remitter (VarT x) = Just x
      remitter _ = Nothing

_ConT :: Prism' Type Name
_ConT
  = prism' reviewer remitter
  where
      reviewer = ConT
      remitter (ConT x) = Just x
      remitter _ = Nothing

_PromotedT :: Prism' Type Name
_PromotedT
  = prism' reviewer remitter
  where
      reviewer = PromotedT
      remitter (PromotedT x) = Just x
      remitter _ = Nothing

_TupleT :: Prism' Type Int
_TupleT
  = prism' reviewer remitter
  where
      reviewer = TupleT
      remitter (TupleT x) = Just x
      remitter _ = Nothing

_UnboxedTupleT :: Prism' Type Int
_UnboxedTupleT
  = prism' reviewer remitter
  where
      reviewer = UnboxedTupleT
      remitter (UnboxedTupleT x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_UnboxedSumT :: Prism' Type SumArity
_UnboxedSumT
  = prism' reviewer remitter
  where
      reviewer = UnboxedSumT
      remitter (UnboxedSumT x) = Just x
      remitter _ = Nothing
#endif

_ArrowT :: Prism' Type ()
_ArrowT
  = prism' reviewer remitter
  where
      reviewer () = ArrowT
      remitter ArrowT = Just ()
      remitter _ = Nothing

_EqualityT :: Prism' Type ()
_EqualityT
  = prism' reviewer remitter
  where
      reviewer () = EqualityT
      remitter EqualityT = Just ()
      remitter _ = Nothing

_ListT :: Prism' Type ()
_ListT
  = prism' reviewer remitter
  where
      reviewer () = ListT
      remitter ListT = Just ()
      remitter _ = Nothing

_PromotedTupleT :: Prism' Type Int
_PromotedTupleT
  = prism' reviewer remitter
  where
      reviewer = PromotedTupleT
      remitter (PromotedTupleT x) = Just x
      remitter _ = Nothing

_PromotedNilT :: Prism' Type ()
_PromotedNilT
  = prism' reviewer remitter
  where
      reviewer () = PromotedNilT
      remitter PromotedNilT = Just ()
      remitter _ = Nothing

_PromotedConsT :: Prism' Type ()
_PromotedConsT
  = prism' reviewer remitter
  where
      reviewer () = PromotedConsT
      remitter PromotedConsT = Just ()
      remitter _ = Nothing

_StarT :: Prism' Type ()
_StarT
  = prism' reviewer remitter
  where
      reviewer () = StarT
      remitter StarT = Just ()
      remitter _ = Nothing

_ConstraintT :: Prism' Type ()
_ConstraintT
  = prism' reviewer remitter
  where
      reviewer () = ConstraintT
      remitter ConstraintT = Just ()
      remitter _ = Nothing

_LitT :: Prism' Type TyLit
_LitT
  = prism' reviewer remitter
  where
      reviewer = LitT
      remitter (LitT x) = Just x
      remitter _ = Nothing

_InfixT :: Prism' Type (Type, Name, Type)
_InfixT
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = InfixT x y z
      remitter (InfixT x y z) = Just (x, y, z)
      remitter _ = Nothing

_UInfixT :: Prism' Type (Type, Name, Type)
_UInfixT
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = UInfixT x y z
      remitter (UInfixT x y z) = Just (x, y, z)
      remitter _ = Nothing

_ParensT :: Prism' Type Type
_ParensT
  = prism' reviewer remitter
  where
      reviewer = ParensT
      remitter (ParensT x) = Just x
      remitter _ = Nothing

_WildCardT :: Prism' Type ()
_WildCardT
  = prism' reviewer remitter
  where
      reviewer () = WildCardT
      remitter WildCardT = Just ()
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,15,0)
_AppKindT :: Prism' Type (Type, Kind)
_AppKindT
  = prism' reviewer remitter
  where
      reviewer (x, y) = AppKindT x y
      remitter (AppKindT x y) = Just (x, y)
      remitter _ = Nothing

_ImplicitParamT :: Prism' Type (String, Type)
_ImplicitParamT
  = prism' reviewer remitter
  where
      reviewer (x, y) = ImplicitParamT x y
      remitter (ImplicitParamT x y) = Just (x, y)
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,16,0)
_ForallVisT :: Prism' Type ([TyVarBndrUnit], Type)
_ForallVisT
  = prism' reviewer remitter
  where
      reviewer (x, y) = ForallVisT x y
      remitter (ForallVisT x y) = Just (x, y)
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,17,0)
_MulArrowT :: Prism' Type ()
_MulArrowT
  = prism' reviewer remitter
  where
      reviewer () = MulArrowT
      remitter MulArrowT = Just ()
      remitter _ = Nothing
#endif

#if MIN_VERSION_template_haskell(2,17,0)
_SpecifiedSpec :: Prism' Specificity ()
_SpecifiedSpec
  = prism' reviewer remitter
  where
      reviewer () = SpecifiedSpec
      remitter SpecifiedSpec = Just ()
      remitter _ = Nothing

_InferredSpec :: Prism' Specificity ()
_InferredSpec
  = prism' reviewer remitter
  where
      reviewer () = InferredSpec
      remitter InferredSpec = Just ()
      remitter _ = Nothing
#endif

-- |
-- @
-- _PlainTV :: 'Prism'' ('TyVarBndr' flag) ('Name', flag) -- template-haskell-2.17+
-- _PlainTV :: 'Prism''  'TyVarBndr'        'Name'        -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,17,0)
_PlainTV :: Prism' (TyVarBndr flag) (Name, flag)
_PlainTV
  = prism' reviewer remitter
  where
      reviewer (x, y) = PlainTV x y
      remitter (PlainTV x y) = Just (x, y)
      remitter _ = Nothing
#else
_PlainTV :: Prism' TyVarBndr Name
_PlainTV
  = prism' reviewer remitter
  where
      reviewer = PlainTV
      remitter (PlainTV x) = Just x
      remitter _ = Nothing
#endif

-- |
-- @
-- _KindedTV :: 'Prism'' ('TyVarBndr' flag) ('Name', flag, 'Kind') -- template-haskell-2.17+
-- _KindedTV :: 'Prism''  'TyVarBndr'       ('Name',       'Kind') -- Earlier versions
-- @
#if MIN_VERSION_template_haskell(2,17,0)
_KindedTV :: Prism' (TyVarBndr flag) (Name, flag, Kind)
_KindedTV
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = KindedTV x y z
      remitter (KindedTV x y z) = Just (x, y, z)
      remitter _ = Nothing
#else
_KindedTV :: Prism' TyVarBndr (Name, Kind)
_KindedTV
  = prism' reviewer remitter
  where
      reviewer (x, y) = KindedTV x y
      remitter (KindedTV x y) = Just (x, y)
      remitter _ = Nothing
#endif

_NoSig :: Prism' FamilyResultSig ()
_NoSig
  = prism' reviewer remitter
  where
      reviewer () = NoSig
      remitter NoSig = Just ()
      remitter _ = Nothing

_KindSig :: Prism' FamilyResultSig Kind
_KindSig
  = prism' reviewer remitter
  where
      reviewer = KindSig
      remitter (KindSig x) = Just x
      remitter _ = Nothing

_TyVarSig :: Prism' FamilyResultSig TyVarBndrUnit
_TyVarSig
  = prism' reviewer remitter
  where
      reviewer = TyVarSig
      remitter (TyVarSig x) = Just x
      remitter _ = Nothing

_NumTyLit :: Prism' TyLit Integer
_NumTyLit
  = prism' reviewer remitter
  where
      reviewer = NumTyLit
      remitter (NumTyLit x) = Just x
      remitter _ = Nothing

_StrTyLit :: Prism' TyLit String
_StrTyLit
  = prism' reviewer remitter
  where
      reviewer = StrTyLit
      remitter (StrTyLit x) = Just x
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,18,0)
_CharTyLit :: Prism' TyLit Char
_CharTyLit
  = prism' reviewer remitter
  where
      reviewer = CharTyLit
      remitter (CharTyLit x) = Just x
      remitter _ = Nothing
#endif

_NominalR :: Prism' Role ()
_NominalR
  = prism' reviewer remitter
  where
      reviewer () = NominalR
      remitter NominalR = Just ()
      remitter _ = Nothing

_RepresentationalR :: Prism' Role ()
_RepresentationalR
  = prism' reviewer remitter
  where
      reviewer () = RepresentationalR
      remitter RepresentationalR = Just ()
      remitter _ = Nothing

_PhantomR :: Prism' Role ()
_PhantomR
  = prism' reviewer remitter
  where
      reviewer () = PhantomR
      remitter PhantomR = Just ()
      remitter _ = Nothing

_InferR :: Prism' Role ()
_InferR
  = prism' reviewer remitter
  where
      reviewer () = InferR
      remitter InferR = Just ()
      remitter _ = Nothing

#if MIN_VERSION_template_haskell(2,12,0)
_StockStrategy :: Prism' DerivStrategy ()
_StockStrategy
  = prism' reviewer remitter
  where
      reviewer () = StockStrategy
      remitter StockStrategy = Just ()
      remitter _ = Nothing

_AnyclassStrategy :: Prism' DerivStrategy ()
_AnyclassStrategy
  = prism' reviewer remitter
  where
      reviewer () = AnyclassStrategy
      remitter AnyclassStrategy = Just ()
      remitter _ = Nothing

_NewtypeStrategy :: Prism' DerivStrategy ()
_NewtypeStrategy
  = prism' reviewer remitter
  where
      reviewer () = NewtypeStrategy
      remitter NewtypeStrategy = Just ()
      remitter _ = Nothing
#endif
