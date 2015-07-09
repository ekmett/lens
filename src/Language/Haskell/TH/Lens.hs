{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Lens
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
-- Lenses, Prisms, and Traversals for working with Template Haskell
----------------------------------------------------------------------------
module Language.Haskell.TH.Lens
  (
  -- * Traversals
    HasName(..)
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
#if MIN_VERSION_template_haskell(2,9,0)
  -- ** TySynEqn Lenses
  , tySynEqnPatterns
  , tySynEqnResult
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
#if MIN_VERSION_template_haskell(2,8,0)
  , _InfixD
#endif
  , _PragmaD
  , _FamilyD
  , _DataInstD
  , _NewtypeInstD
  , _TySynInstD
#if MIN_VERSION_template_haskell(2,9,0)
  , _ClosedTypeFamilyD
  , _RoleAnnotD
#endif
  -- ** Con Prisms
  , _NormalC
  , _RecC
  , _InfixC
  , _ForallC
  -- ** Strict Prisms
  , _IsStrict
  , _NotStrict
  , _Unpacked
  -- ** Foreign Prisms
  , _ImportF
  , _ExportF
  -- ** Callconv Prisms
  , _CCall
  , _StdCall
  -- ** Safety Prisms
  , _Unsafe
  , _Safe
  , _Interruptible
  -- ** Pragma Prisms
  , _InlineP
  , _SpecialiseP
#if MIN_VERSION_template_haskell(2,8,0)
  , _SpecialiseInstP
  , _RuleP
#if MIN_VERSION_template_haskell(2,9,0)
  , _AnnP
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
#endif
#if MIN_VERSION_template_haskell(2,9,0)
  -- ** AnnTarget Prisms
  , _ModuleAnnotation
  , _TypeAnnotation
  , _ValueAnnotation
#endif
  -- ** FunDep Prisms TODO make a lens
  , _FunDep
  -- ** FamFlavour Prisms
  , _TypeFam
  , _DataFam
  -- ** FixityDirection Prisms
  , _InfixL
  , _InfixR
  , _InfixN
  -- ** Exp Prisms
  , _VarE
  , _ConE
  , _LitE
  , _AppE
  , _InfixE
  , _UInfixE
  , _ParensE
  , _LamE
#if MIN_VERSION_template_haskell(2,8,0)
  , _LamCaseE
#endif
  , _TupE
  , _UnboxedTupE
  , _CondE
#if MIN_VERSION_template_haskell(2,8,0)
  , _MultiIfE
#endif
  , _LetE
  , _CaseE
  , _DoE
  , _CompE
  , _ArithSeqE
  , _ListE
  , _SigE
  , _RecConE
  , _RecUpdE
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
  -- ** Pat Prisms
  , _LitP
  , _VarP
  , _TupP
  , _UnboxedTupP
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
#if MIN_VERSION_template_haskell(2,8,0)
  , _PromotedT
#endif
  , _TupleT
  , _UnboxedTupleT
  , _ArrowT
  , _ListT
#if MIN_VERSION_template_haskell(2,8,0)
  , _PromotedTupleT
  , _PromotedNilT
  , _PromotedConsT
  , _StarT
  , _ConstraintT
  , _LitT
#endif
  -- ** TyVarBndr Prisms
  , _PlainTV
  , _KindedTV
#if MIN_VERSION_template_haskell(2,8,0)
  -- ** TyLit Prisms
  , _NumTyLit
  , _StrTyLit
#endif
#if !MIN_VERSION_template_haskell(2,10,0)
  -- ** Pred Prisms
  , _ClassP
  , _EqualP
#endif
#if MIN_VERSION_template_haskell(2,9,0)
  -- ** Role Prisms
  , _NominalR
  , _RepresentationalR
#endif
  ) where

import Control.Applicative
import Control.Lens.At
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Fold
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.Tuple
import Control.Lens.Traversal
import Data.Map as Map hiding (toList,map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set as Set hiding (toList,map)
import Data.Set.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#if MIN_VERSION_template_haskell(2,8,0)
import Data.Word
#endif
import Prelude

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Lens' t Name

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

-- type VarStrictType = (Name, Strict, Type)
instance HasName (Name, a, b) where
  name f (n, s, t) = (,,) <$> f n <*> pure s <*> pure t


-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- | When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

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

#if !MIN_VERSION_template_haskell(2,10,0)
instance HasTypeVars Pred where
  typeVarsEx s f (ClassP n ts) = ClassP n <$> typeVarsEx s f ts
  typeVarsEx s f (EqualP l r)  = EqualP <$> typeVarsEx s f l <*> typeVarsEx s f r
#endif

instance HasTypeVars Con where
  typeVarsEx s f (NormalC n ts) = NormalC n <$> traverseOf (traverse . _2) (typeVarsEx s f) ts
  typeVarsEx s f (RecC n ts) = RecC n <$> traverseOf (traverse . _3) (typeVarsEx s f) ts
  typeVarsEx s f (InfixC l n r) = InfixC <$> g l <*> pure n <*> g r
       where g (i, t) = (,) i <$> typeVarsEx s f t
  typeVarsEx s f (ForallC bs ctx c) = ForallC bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f c
       where s' = s `Set.union` setOf typeVars bs

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
  substType m t@(VarT n)          = fromMaybe t (m^.at n)
  substType m (ForallT bs ctx ty) = ForallT bs (substType m' ctx) (substType m' ty)
    where m' = foldrOf typeVars Map.delete m bs
  substType m (SigT t k)          = SigT (substType m t) k
  substType m (AppT l r)          = AppT (substType m l) (substType m r)
  substType _ t                   = t

instance SubstType t => SubstType [t] where
  substType = map . substType

#if !MIN_VERSION_template_haskell(2,10,0)
instance SubstType Pred where
  substType m (ClassP n ts) = ClassP n (substType m ts)
  substType m (EqualP l r)  = substType m (EqualP l r)
#endif

-- | Provides a 'Traversal' of the types of each field of a constructor.
conFields :: Traversal' Con StrictType
conFields f (NormalC n fs)      = NormalC n <$> traverse f fs
conFields f (RecC n fs)         = RecC n <$> traverse sans_var fs
  where sans_var (fn,s,t) = (\(s', t') -> (fn,s',t')) <$> f (s, t)
conFields f (InfixC l n r)      = InfixC <$> f l <*> pure n <*> f r
conFields f (ForallC bds ctx c) = ForallC bds ctx <$> conFields f c

-- | 'Traversal' of the types of the /named/ fields of a constructor.
conNamedFields :: Traversal' Con VarStrictType
conNamedFields f (RecC n fs) = RecC n <$> traverse f fs
conNamedFields f (ForallC a b fs) = ForallC a b <$> conNamedFields f fs
conNamedFields _ c = pure c

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

#if MIN_VERSION_template_haskell(2,8,0)
_ClassI :: Prism' Info (Dec, [InstanceDec])
_ClassI
  = prism remitter reviewer
  where
      remitter (x, y) = ClassI x y
      reviewer (ClassI x y) = Right (x, y)
      reviewer x = Left x

_ClassOpI :: Prism' Info (Name, Type, ParentName, Fixity)
_ClassOpI
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = ClassOpI x y z w
      reviewer (ClassOpI x y z w) = Right (x, y, z, w)
      reviewer x = Left x

#else
_ClassI :: Prism' Info (Dec, [Dec])
_ClassI
  = prism remitter reviewer
  where
      remitter (x, y) = ClassI x y
      reviewer (ClassI x y) = Right (x, y)
      reviewer x = Left x

_ClassOpI :: Prism' Info (Name, Type, Name, Fixity)
_ClassOpI
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = ClassOpI x y z w
      reviewer (ClassOpI x y z w) = Right (x, y, z, w)
      reviewer x = Left x
#endif

_TyConI :: Prism' Info Dec
_TyConI
  = prism remitter reviewer
  where
      remitter = TyConI
      reviewer (TyConI x) = Right x
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_FamilyI :: Prism' Info (Dec, [InstanceDec])
_FamilyI
  = prism remitter reviewer
  where
      remitter (x, y) = FamilyI x y
      reviewer (FamilyI x y) = Right (x, y)
      reviewer x = Left x

_PrimTyConI :: Prism' Info (Name, Arity, Unlifted)
_PrimTyConI
  = prism remitter reviewer
  where
      remitter (x, y, z) = PrimTyConI x y z
      reviewer (PrimTyConI x y z) = Right (x, y, z)
      reviewer x = Left x

_DataConI :: Prism' Info (Name, Type, ParentName, Fixity)
_DataConI
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = DataConI x y z w
      reviewer (DataConI x y z w) = Right (x, y, z, w)
      reviewer x = Left x
#else
_FamilyI :: Prism' Info (Dec, [Dec])
_FamilyI
  = prism remitter reviewer
  where
      remitter (x, y) = FamilyI x y
      reviewer (FamilyI x y) = Right (x, y)
      reviewer x = Left x

_PrimTyConI :: Prism' Info (Name, Int, Bool)
_PrimTyConI
  = prism remitter reviewer
  where
      remitter (x, y, z) = PrimTyConI x y z
      reviewer (PrimTyConI x y z) = Right (x, y, z)
      reviewer x = Left x

_DataConI :: Prism' Info (Name, Type, Name, Fixity)
_DataConI
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = DataConI x y z w
      reviewer (DataConI x y z w) = Right (x, y, z, w)
      reviewer x = Left x

#endif

_VarI :: Prism' Info (Name, Type, Maybe Dec, Fixity)
_VarI
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = VarI x y z w
      reviewer (VarI x y z w) = Right (x, y, z, w)
      reviewer x = Left x

_TyVarI :: Prism' Info (Name, Type)
_TyVarI
  = prism remitter reviewer
  where
      remitter (x, y) = TyVarI x y
      reviewer (TyVarI x y) = Right (x, y)
      reviewer x = Left x

_FunD :: Prism' Dec (Name, [Clause])
_FunD
  = prism remitter reviewer
  where
      remitter (x, y) = FunD x y
      reviewer (FunD x y) = Right (x, y)
      reviewer x = Left x

_ValD :: Prism' Dec (Pat, Body, [Dec])
_ValD
  = prism remitter reviewer
  where
      remitter (x, y, z) = ValD x y z
      reviewer (ValD x y z) = Right (x, y, z)
      reviewer x = Left x

_DataD :: Prism' Dec (Cxt, Name, [TyVarBndr], [Con], [Name])
_DataD
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u) = DataD x y z w u
      reviewer (DataD x y z w u) = Right (x, y, z, w, u)
      reviewer x = Left x

_NewtypeD :: Prism' Dec (Cxt, Name, [TyVarBndr], Con, [Name])
_NewtypeD
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u) = NewtypeD x y z w u
      reviewer (NewtypeD x y z w u) = Right (x, y, z, w, u)
      reviewer x = Left x

_TySynD :: Prism' Dec (Name, [TyVarBndr], Type)
_TySynD
  = prism remitter reviewer
  where
      remitter (x, y, z) = TySynD x y z
      reviewer (TySynD x y z) = Right (x, y, z)
      reviewer x = Left x

_ClassD :: Prism' Dec (Cxt, Name, [TyVarBndr], [FunDep], [Dec])
_ClassD
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u) = ClassD x y z w u
      reviewer (ClassD x y z w u) = Right (x, y, z, w, u)
      reviewer x = Left x

_InstanceD :: Prism' Dec (Cxt, Type, [Dec])
_InstanceD
  = prism remitter reviewer
  where
      remitter (x, y, z) = InstanceD x y z
      reviewer (InstanceD x y z) = Right (x, y, z)
      reviewer x = Left x

_SigD :: Prism' Dec (Name, Type)
_SigD
  = prism remitter reviewer
  where
      remitter (x, y) = SigD x y
      reviewer (SigD x y) = Right (x, y)
      reviewer x = Left x

_ForeignD :: Prism' Dec Foreign
_ForeignD
  = prism remitter reviewer
  where
      remitter = ForeignD
      reviewer (ForeignD x) = Right x
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_InfixD :: Prism' Dec (Fixity, Name)
_InfixD
  = prism remitter reviewer
  where
      remitter (x, y) = InfixD x y
      reviewer (InfixD x y) = Right (x, y)
      reviewer x = Left x
#endif

_PragmaD :: Prism' Dec Pragma
_PragmaD
  = prism remitter reviewer
  where
      remitter = PragmaD
      reviewer (PragmaD x) = Right x
      reviewer x = Left x

_FamilyD :: Prism' Dec (FamFlavour, Name, [TyVarBndr], Maybe Kind)
_FamilyD
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = FamilyD x y z w
      reviewer (FamilyD x y z w) = Right (x, y, z, w)
      reviewer x = Left x

_DataInstD :: Prism' Dec (Cxt, Name, [Type], [Con], [Name])
_DataInstD
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u) = DataInstD x y z w u
      reviewer (DataInstD x y z w u) = Right (x, y, z, w, u)
      reviewer x = Left x

_NewtypeInstD :: Prism' Dec (Cxt, Name, [Type], Con, [Name])
_NewtypeInstD
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u) = NewtypeInstD x y z w u
      reviewer (NewtypeInstD x y z w u) = Right (x, y, z, w, u)
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,9,0)
_TySynInstD :: Prism' Dec (Name, TySynEqn)
_TySynInstD
  = prism remitter reviewer
  where
      remitter (x, y) = TySynInstD x y
      reviewer (TySynInstD x y) = Right (x, y)
      reviewer x = Left x

_ClosedTypeFamilyD :: Prism' Dec (Name, [TyVarBndr], Maybe Kind, [TySynEqn])
_ClosedTypeFamilyD
  = prism remitter reviewer
  where
      remitter (x, y, z, w) = ClosedTypeFamilyD x y z w
      reviewer (ClosedTypeFamilyD x y z w) = Right (x, y, z, w)
      reviewer x = Left x

_RoleAnnotD :: Prism' Dec (Name, [Role])
_RoleAnnotD
  = prism remitter reviewer
  where
      remitter (x, y) = RoleAnnotD x y
      reviewer (RoleAnnotD x y) = Right (x, y)
      reviewer x = Left x

#else
_TySynInstD :: Prism' Dec (Name, [Type], Type)
_TySynInstD
  = prism remitter reviewer
  where
      remitter (x, y, z) = TySynInstD x y z
      reviewer (TySynInstD x y z) = Right (x, y, z)
      reviewer x = Left x
#endif

_NormalC ::
  Prism' Con (Name, [StrictType])
_NormalC
  = prism remitter reviewer
  where
      remitter (x, y) = NormalC x y
      reviewer (NormalC x y) = Right (x, y)
      reviewer x = Left x

_RecC ::
  Prism' Con (Name, [VarStrictType])
_RecC
  = prism remitter reviewer
  where
      remitter (x, y) = RecC x y
      reviewer (RecC x y) = Right (x, y)
      reviewer x = Left x

_InfixC ::
  Prism' Con (StrictType,
              Name,
              StrictType)
_InfixC
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = InfixC x y z
      reviewer (InfixC x y z)
        = Right (x, y, z)
      reviewer x = Left x

_ForallC :: Prism' Con ([TyVarBndr], Cxt, Con)
_ForallC
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = ForallC x y z
      reviewer (ForallC x y z)
        = Right (x, y, z)
      reviewer x = Left x

_IsStrict :: Prism' Strict ()
_IsStrict
  = prism remitter reviewer
  where
      remitter () = IsStrict
      reviewer IsStrict = Right ()
      reviewer x = Left x

_NotStrict :: Prism' Strict ()
_NotStrict
  = prism remitter reviewer
  where
      remitter () = NotStrict
      reviewer NotStrict = Right ()
      reviewer x = Left x

_Unpacked :: Prism' Strict ()
_Unpacked
  = prism remitter reviewer
  where
      remitter () = Unpacked
      reviewer Unpacked = Right ()
      reviewer x = Left x

_ImportF :: Prism' Foreign (Callconv, Safety, String, Name, Type)
_ImportF
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u)
        = ImportF x y z w u
      reviewer (ImportF x y z w u)
        = Right (x, y, z, w, u)
      reviewer x = Left x

_ExportF :: Prism' Foreign (Callconv, String, Name, Type)
_ExportF
  = prism remitter reviewer
  where
      remitter (x, y, z, w)
        = ExportF x y z w
      reviewer (ExportF x y z w)
        = Right (x, y, z, w)
      reviewer x = Left x

_CCall :: Prism' Callconv ()
_CCall
  = prism remitter reviewer
  where
      remitter () = CCall
      reviewer CCall = Right ()
      reviewer x = Left x

_StdCall :: Prism' Callconv ()
_StdCall
  = prism remitter reviewer
  where
      remitter () = StdCall
      reviewer StdCall = Right ()
      reviewer x = Left x

_Unsafe :: Prism' Safety ()
_Unsafe
  = prism remitter reviewer
  where
      remitter () = Unsafe
      reviewer Unsafe = Right ()
      reviewer x = Left x

_Safe :: Prism' Safety ()
_Safe
  = prism remitter reviewer
  where
      remitter () = Safe
      reviewer Safe = Right ()
      reviewer x = Left x

_Interruptible :: Prism' Safety ()
_Interruptible
  = prism remitter reviewer
  where
      remitter () = Interruptible
      reviewer Interruptible = Right ()
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_InlineP :: Prism' Pragma (Name, Inline, RuleMatch, Phases)
_InlineP
  = prism remitter reviewer
  where
      remitter (x, y, z, w)
        = InlineP x y z w
      reviewer (InlineP x y z w)
        = Right (x, y, z, w)
      reviewer x = Left x

_SpecialiseP :: Prism' Pragma (Name, Type, Maybe Inline, Phases)
_SpecialiseP
  = prism remitter reviewer
  where
      remitter (x, y, z, w)
        = SpecialiseP x y z w
      reviewer (SpecialiseP x y z w)
        = Right (x, y, z, w)
      reviewer x = Left x
#else
_InlineP :: Prism' Pragma (Name, InlineSpec)
_InlineP
  = prism remitter reviewer
  where
      remitter (x, y)
        = InlineP x y
      reviewer (InlineP x y)
        = Right (x, y)
      reviewer x = Left x

_SpecialiseP :: Prism' Pragma (Name, Type, Maybe InlineSpec)
_SpecialiseP
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = SpecialiseP x y z
      reviewer (SpecialiseP x y z)
        = Right (x, y, z)
      reviewer x = Left x

-- TODO add lenses for InlineSpec
#endif

#if MIN_VERSION_template_haskell(2,8,0)
_SpecialiseInstP :: Prism' Pragma Type
_SpecialiseInstP
  = prism remitter reviewer
  where
      remitter = SpecialiseInstP
      reviewer (SpecialiseInstP x) = Right x
      reviewer x = Left x

_RuleP :: Prism' Pragma (String, [RuleBndr], Exp, Exp, Phases)
_RuleP
  = prism remitter reviewer
  where
      remitter (x, y, z, w, u)
        = RuleP x y z w u
      reviewer (RuleP x y z w u)
        = Right (x, y, z, w, u)
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,9,0)
_AnnP :: Prism' Pragma (AnnTarget, Exp)
_AnnP
  = prism remitter reviewer
  where
      remitter (x, y) = AnnP x y
      reviewer (AnnP x y) = Right (x, y)
      reviewer x = Left x
#endif

_NoInline :: Prism' Inline ()
_NoInline
  = prism remitter reviewer
  where
      remitter () = NoInline
      reviewer NoInline = Right ()
      reviewer x = Left x

_Inline :: Prism' Inline ()
_Inline
  = prism remitter reviewer
  where
      remitter () = Inline
      reviewer Inline = Right ()
      reviewer x = Left x

_Inlinable :: Prism' Inline ()
_Inlinable
  = prism remitter reviewer
  where
      remitter () = Inlinable
      reviewer Inlinable = Right ()
      reviewer x = Left x

_ConLike :: Prism' RuleMatch ()
_ConLike
  = prism remitter reviewer
  where
      remitter () = ConLike
      reviewer ConLike = Right ()
      reviewer x = Left x

_FunLike :: Prism' RuleMatch ()
_FunLike
  = prism remitter reviewer
  where
      remitter () = FunLike
      reviewer FunLike = Right ()
      reviewer x = Left x

_AllPhases :: Prism' Phases ()
_AllPhases
  = prism remitter reviewer
  where
      remitter () = AllPhases
      reviewer AllPhases = Right ()
      reviewer x = Left x

_FromPhase :: Prism' Phases Int
_FromPhase
  = prism remitter reviewer
  where
      remitter = FromPhase
      reviewer (FromPhase x) = Right x
      reviewer x = Left x

_BeforePhase :: Prism' Phases Int
_BeforePhase
  = prism remitter reviewer
  where
      remitter = BeforePhase
      reviewer (BeforePhase x) = Right x
      reviewer x = Left x

_RuleVar :: Prism' RuleBndr Name
_RuleVar
  = prism remitter reviewer
  where
      remitter = RuleVar
      reviewer (RuleVar x) = Right x
      reviewer x = Left x

_TypedRuleVar :: Prism' RuleBndr (Name, Type)
_TypedRuleVar
  = prism remitter reviewer
  where
      remitter (x, y) = TypedRuleVar x y
      reviewer (TypedRuleVar x y) = Right (x, y)
      reviewer x = Left x
#endif

#if MIN_VERSION_template_haskell(2,9,0)
_ModuleAnnotation :: Prism' AnnTarget ()
_ModuleAnnotation
  = prism remitter reviewer
  where
      remitter () = ModuleAnnotation
      reviewer ModuleAnnotation
        = Right ()
      reviewer x = Left x

_TypeAnnotation :: Prism' AnnTarget Name
_TypeAnnotation
  = prism remitter reviewer
  where
      remitter = TypeAnnotation
      reviewer (TypeAnnotation x)
        = Right x
      reviewer x = Left x

_ValueAnnotation :: Prism' AnnTarget Name
_ValueAnnotation
  = prism remitter reviewer
  where
      remitter = ValueAnnotation
      reviewer (ValueAnnotation x) = Right x
      reviewer x = Left x
#endif

_FunDep :: Prism' FunDep ([Name], [Name])
_FunDep
  = prism remitter reviewer
  where
      remitter (x, y) = FunDep x y
      reviewer (FunDep x y) = Right (x, y)

_TypeFam :: Prism' FamFlavour ()
_TypeFam
  = prism remitter reviewer
  where
      remitter () = TypeFam
      reviewer TypeFam = Right ()
      reviewer x = Left x

_DataFam :: Prism' FamFlavour ()
_DataFam
  = prism remitter reviewer
  where
      remitter () = DataFam
      reviewer DataFam = Right ()
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,9,0)
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
  = prism remitter reviewer
  where
      remitter () = InfixL
      reviewer InfixL = Right ()
      reviewer x = Left x

_InfixR :: Prism' FixityDirection ()
_InfixR
  = prism remitter reviewer
  where
      remitter () = InfixR
      reviewer InfixR = Right ()
      reviewer x = Left x

_InfixN :: Prism' FixityDirection ()
_InfixN
  = prism remitter reviewer
  where
      remitter () = InfixN
      reviewer InfixN = Right ()
      reviewer x = Left x

_VarE :: Prism' Exp Name
_VarE
  = prism remitter reviewer
  where
      remitter = VarE
      reviewer (VarE x) = Right x
      reviewer x = Left x

_ConE :: Prism' Exp Name
_ConE
  = prism remitter reviewer
  where
      remitter = ConE
      reviewer (ConE x) = Right x
      reviewer x = Left x

_LitE :: Prism' Exp Lit
_LitE
  = prism remitter reviewer
  where
      remitter = LitE
      reviewer (LitE x) = Right x
      reviewer x = Left x

_AppE :: Prism' Exp (Exp, Exp)
_AppE
  = prism remitter reviewer
  where
      remitter (x, y) = AppE x y
      reviewer (AppE x y) = Right (x, y)
      reviewer x = Left x

_InfixE :: Prism' Exp (Maybe Exp, Exp, Maybe Exp)
_InfixE
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = InfixE x y z
      reviewer (InfixE x y z)
        = Right (x, y, z)
      reviewer x = Left x

_UInfixE :: Prism' Exp (Exp, Exp, Exp)
_UInfixE
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = UInfixE x y z
      reviewer (UInfixE x y z)
        = Right (x, y, z)
      reviewer x = Left x

_ParensE :: Prism' Exp Exp
_ParensE
  = prism remitter reviewer
  where
      remitter = ParensE
      reviewer (ParensE x) = Right x
      reviewer x = Left x

_LamE :: Prism' Exp ([Pat], Exp)
_LamE
  = prism remitter reviewer
  where
      remitter (x, y) = LamE x y
      reviewer (LamE x y) = Right (x, y)
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_LamCaseE :: Prism' Exp [Match]
_LamCaseE
  = prism remitter reviewer
  where
      remitter = LamCaseE
      reviewer (LamCaseE x) = Right x
      reviewer x = Left x
#endif

_TupE :: Prism' Exp [Exp]
_TupE
  = prism remitter reviewer
  where
      remitter = TupE
      reviewer (TupE x) = Right x
      reviewer x = Left x

_UnboxedTupE :: Prism' Exp [Exp]
_UnboxedTupE
  = prism remitter reviewer
  where
      remitter = UnboxedTupE
      reviewer (UnboxedTupE x) = Right x
      reviewer x = Left x

_CondE :: Prism' Exp (Exp, Exp, Exp)
_CondE
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = CondE x y z
      reviewer (CondE x y z)
        = Right (x, y, z)
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_MultiIfE :: Prism' Exp [(Guard, Exp)]
_MultiIfE
  = prism remitter reviewer
  where
      remitter = MultiIfE
      reviewer (MultiIfE x) = Right x
      reviewer x = Left x
#endif

_LetE :: Prism' Exp ([Dec], Exp)
_LetE
  = prism remitter reviewer
  where
      remitter (x, y) = LetE x y
      reviewer (LetE x y) = Right (x, y)
      reviewer x = Left x

_CaseE :: Prism' Exp (Exp, [Match])
_CaseE
  = prism remitter reviewer
  where
      remitter (x, y) = CaseE x y
      reviewer (CaseE x y) = Right (x, y)
      reviewer x = Left x

_DoE :: Prism' Exp [Stmt]
_DoE
  = prism remitter reviewer
  where
      remitter = DoE
      reviewer (DoE x) = Right x
      reviewer x = Left x

_CompE :: Prism' Exp [Stmt]
_CompE
  = prism remitter reviewer
  where
      remitter = CompE
      reviewer (CompE x) = Right x
      reviewer x = Left x

_ArithSeqE :: Prism' Exp Range
_ArithSeqE
  = prism remitter reviewer
  where
      remitter = ArithSeqE
      reviewer (ArithSeqE x) = Right x
      reviewer x = Left x

_ListE :: Prism' Exp [Exp]
_ListE
  = prism remitter reviewer
  where
      remitter = ListE
      reviewer (ListE x) = Right x
      reviewer x = Left x

_SigE :: Prism' Exp (Exp, Type)
_SigE
  = prism remitter reviewer
  where
      remitter (x, y) = SigE x y
      reviewer (SigE x y) = Right (x, y)
      reviewer x = Left x

_RecConE :: Prism' Exp (Name, [FieldExp])
_RecConE
  = prism remitter reviewer
  where
      remitter (x, y) = RecConE x y
      reviewer (RecConE x y) = Right (x, y)
      reviewer x = Left x

_RecUpdE :: Prism' Exp (Exp, [FieldExp])
_RecUpdE
  = prism remitter reviewer
  where
      remitter (x, y) = RecUpdE x y
      reviewer (RecUpdE x y) = Right (x, y)
      reviewer x = Left x

_GuardedB :: Prism' Body [(Guard, Exp)]
_GuardedB
  = prism remitter reviewer
  where
      remitter = GuardedB
      reviewer (GuardedB x) = Right x
      reviewer x = Left x

_NormalB :: Prism' Body Exp
_NormalB
  = prism remitter reviewer
  where
      remitter = NormalB
      reviewer (NormalB x) = Right x
      reviewer x = Left x

_NormalG :: Prism' Guard Exp
_NormalG
  = prism remitter reviewer
  where
      remitter = NormalG
      reviewer (NormalG x) = Right x
      reviewer x = Left x

_PatG :: Prism' Guard [Stmt]
_PatG
  = prism remitter reviewer
  where
      remitter = PatG
      reviewer (PatG x) = Right x
      reviewer x = Left x

_BindS :: Prism' Stmt (Pat, Exp)
_BindS
  = prism remitter reviewer
  where
      remitter (x, y) = BindS x y
      reviewer (BindS x y) = Right (x, y)
      reviewer x = Left x

_LetS :: Prism' Stmt [Dec]
_LetS
  = prism remitter reviewer
  where
      remitter = LetS
      reviewer (LetS x) = Right x
      reviewer x = Left x

_NoBindS :: Prism' Stmt Exp
_NoBindS
  = prism remitter reviewer
  where
      remitter = NoBindS
      reviewer (NoBindS x) = Right x
      reviewer x = Left x

_ParS :: Prism' Stmt [[Stmt]]
_ParS
  = prism remitter reviewer
  where
      remitter = ParS
      reviewer (ParS x) = Right x
      reviewer x = Left x

_FromR :: Prism' Range Exp
_FromR
  = prism remitter reviewer
  where
      remitter = FromR
      reviewer (FromR x) = Right x
      reviewer x = Left x

_FromThenR :: Prism' Range (Exp, Exp)
_FromThenR
  = prism remitter reviewer
  where
      remitter (x, y) = FromThenR x y
      reviewer (FromThenR x y)
        = Right (x, y)
      reviewer x = Left x

_FromToR :: Prism' Range (Exp, Exp)
_FromToR
  = prism remitter reviewer
  where
      remitter (x, y) = FromToR x y
      reviewer (FromToR x y) = Right (x, y)
      reviewer x = Left x

_FromThenToR :: Prism' Range (Exp, Exp, Exp)
_FromThenToR
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = FromThenToR x y z
      reviewer (FromThenToR x y z)
        = Right (x, y, z)
      reviewer x = Left x

_CharL :: Prism' Lit Char
_CharL
  = prism remitter reviewer
  where
      remitter = CharL
      reviewer (CharL x) = Right x
      reviewer x = Left x

_StringL :: Prism' Lit String
_StringL
  = prism remitter reviewer
  where
      remitter = StringL
      reviewer (StringL x) = Right x
      reviewer x = Left x

_IntegerL :: Prism' Lit Integer
_IntegerL
  = prism remitter reviewer
  where
      remitter = IntegerL
      reviewer (IntegerL x) = Right x
      reviewer x = Left x

_RationalL :: Prism' Lit Rational
_RationalL
  = prism remitter reviewer
  where
      remitter = RationalL
      reviewer (RationalL x) = Right x
      reviewer x = Left x

_IntPrimL :: Prism' Lit Integer
_IntPrimL
  = prism remitter reviewer
  where
      remitter = IntPrimL
      reviewer (IntPrimL x) = Right x
      reviewer x = Left x

_WordPrimL :: Prism' Lit Integer
_WordPrimL
  = prism remitter reviewer
  where
      remitter = WordPrimL
      reviewer (WordPrimL x) = Right x
      reviewer x = Left x

_FloatPrimL :: Prism' Lit Rational
_FloatPrimL
  = prism remitter reviewer
  where
      remitter = FloatPrimL
      reviewer (FloatPrimL x) = Right x
      reviewer x = Left x

_DoublePrimL :: Prism' Lit Rational
_DoublePrimL
  = prism remitter reviewer
  where
      remitter = DoublePrimL
      reviewer (DoublePrimL x) = Right x
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_StringPrimL :: Prism' Lit [Word8]
_StringPrimL
  = prism remitter reviewer
  where
      remitter = StringPrimL
      reviewer (StringPrimL x) = Right x
      reviewer x = Left x
#else
_StringPrimL :: Prism' Lit String
_StringPrimL
  = prism remitter reviewer
  where
      remitter = StringPrimL
      reviewer (StringPrimL x) = Right x
      reviewer x = Left x
#endif

_LitP :: Prism' Pat Lit
_LitP
  = prism remitter reviewer
  where
      remitter = LitP
      reviewer (LitP x) = Right x
      reviewer x = Left x

_VarP :: Prism' Pat Name
_VarP
  = prism remitter reviewer
  where
      remitter = VarP
      reviewer (VarP x) = Right x
      reviewer x = Left x

_TupP :: Prism' Pat [Pat]
_TupP
  = prism remitter reviewer
  where
      remitter = TupP
      reviewer (TupP x) = Right x
      reviewer x = Left x

_UnboxedTupP :: Prism' Pat [Pat]
_UnboxedTupP
  = prism remitter reviewer
  where
      remitter = UnboxedTupP
      reviewer (UnboxedTupP x) = Right x
      reviewer x = Left x

_ConP :: Prism' Pat (Name, [Pat])
_ConP
  = prism remitter reviewer
  where
      remitter (x, y) = ConP x y
      reviewer (ConP x y) = Right (x, y)
      reviewer x = Left x

_InfixP :: Prism' Pat (Pat, Name, Pat)
_InfixP
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = InfixP x y z
      reviewer (InfixP x y z)
        = Right (x, y, z)
      reviewer x = Left x
_UInfixP :: Prism' Pat (Pat, Name, Pat)
_UInfixP
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = UInfixP x y z
      reviewer (UInfixP x y z)
        = Right (x, y, z)
      reviewer x = Left x

_ParensP :: Prism' Pat Pat
_ParensP
  = prism remitter reviewer
  where
      remitter = ParensP
      reviewer (ParensP x) = Right x
      reviewer x = Left x

_TildeP :: Prism' Pat Pat
_TildeP
  = prism remitter reviewer
  where
      remitter = TildeP
      reviewer (TildeP x) = Right x
      reviewer x = Left x

_BangP :: Prism' Pat Pat
_BangP
  = prism remitter reviewer
  where
      remitter = BangP
      reviewer (BangP x) = Right x
      reviewer x = Left x

_AsP :: Prism' Pat (Name, Pat)
_AsP
  = prism remitter reviewer
  where
      remitter (x, y) = AsP x y
      reviewer (AsP x y) = Right (x, y)
      reviewer x = Left x

_WildP :: Prism' Pat ()
_WildP
  = prism remitter reviewer
  where
      remitter () = WildP
      reviewer WildP = Right ()
      reviewer x = Left x

_RecP :: Prism' Pat (Name, [FieldPat])
_RecP
  = prism remitter reviewer
  where
      remitter (x, y) = RecP x y
      reviewer (RecP x y) = Right (x, y)
      reviewer x = Left x

_ListP :: Prism' Pat [Pat]
_ListP
  = prism remitter reviewer
  where
      remitter = ListP
      reviewer (ListP x) = Right x
      reviewer x = Left x

_SigP :: Prism' Pat (Pat, Type)
_SigP
  = prism remitter reviewer
  where
      remitter (x, y) = SigP x y
      reviewer (SigP x y) = Right (x, y)
      reviewer x = Left x

_ViewP :: Prism' Pat (Exp, Pat)
_ViewP
  = prism remitter reviewer
  where
      remitter (x, y) = ViewP x y
      reviewer (ViewP x y) = Right (x, y)
      reviewer x = Left x

_ForallT :: Prism' Type ([TyVarBndr], Cxt, Type)
_ForallT
  = prism remitter reviewer
  where
      remitter (x, y, z)
        = ForallT x y z
      reviewer (ForallT x y z)
        = Right (x, y, z)
      reviewer x = Left x

_AppT :: Prism' Type (Type, Type)
_AppT
  = prism remitter reviewer
  where
      remitter (x, y) = AppT x y
      reviewer (AppT x y) = Right (x, y)
      reviewer x = Left x

_SigT :: Prism' Type (Type, Kind)
_SigT
  = prism remitter reviewer
  where
      remitter (x, y) = SigT x y
      reviewer (SigT x y) = Right (x, y)
      reviewer x = Left x

_VarT :: Prism' Type Name
_VarT
  = prism remitter reviewer
  where
      remitter = VarT
      reviewer (VarT x) = Right x
      reviewer x = Left x

_ConT :: Prism' Type Name
_ConT
  = prism remitter reviewer
  where
      remitter = ConT
      reviewer (ConT x) = Right x
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_PromotedT :: Prism' Type Name
_PromotedT
  = prism remitter reviewer
  where
      remitter = PromotedT
      reviewer (PromotedT x) = Right x
      reviewer x = Left x
#endif

_TupleT :: Prism' Type Int
_TupleT
  = prism remitter reviewer
  where
      remitter = TupleT
      reviewer (TupleT x) = Right x
      reviewer x = Left x

_UnboxedTupleT :: Prism' Type Int
_UnboxedTupleT
  = prism remitter reviewer
  where
      remitter = UnboxedTupleT
      reviewer (UnboxedTupleT x) = Right x
      reviewer x = Left x

_ArrowT :: Prism' Type ()
_ArrowT
  = prism remitter reviewer
  where
      remitter () = ArrowT
      reviewer ArrowT = Right ()
      reviewer x = Left x

_ListT :: Prism' Type ()
_ListT
  = prism remitter reviewer
  where
      remitter () = ListT
      reviewer ListT = Right ()
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_PromotedTupleT :: Prism' Type Int
_PromotedTupleT
  = prism remitter reviewer
  where
      remitter = PromotedTupleT
      reviewer (PromotedTupleT x) = Right x
      reviewer x = Left x

_PromotedNilT :: Prism' Type ()
_PromotedNilT
  = prism remitter reviewer
  where
      remitter () = PromotedNilT
      reviewer PromotedNilT = Right ()
      reviewer x = Left x

_PromotedConsT :: Prism' Type ()
_PromotedConsT
  = prism remitter reviewer
  where
      remitter () = PromotedConsT
      reviewer PromotedConsT = Right ()
      reviewer x = Left x

_StarT :: Prism' Type ()
_StarT
  = prism remitter reviewer
  where
      remitter () = StarT
      reviewer StarT = Right ()
      reviewer x = Left x

_ConstraintT :: Prism' Type ()
_ConstraintT
  = prism remitter reviewer
  where
      remitter () = ConstraintT
      reviewer ConstraintT = Right ()
      reviewer x = Left x

_LitT :: Prism' Type TyLit
_LitT
  = prism remitter reviewer
  where
      remitter = LitT
      reviewer (LitT x) = Right x
      reviewer x = Left x
#endif

_PlainTV :: Prism' TyVarBndr Name
_PlainTV
  = prism remitter reviewer
  where
      remitter = PlainTV
      reviewer (PlainTV x) = Right x
      reviewer x = Left x

_KindedTV :: Prism' TyVarBndr (Name, Kind)
_KindedTV
  = prism remitter reviewer
  where
      remitter (x, y) = KindedTV x y
      reviewer (KindedTV x y) = Right (x, y)
      reviewer x = Left x

#if MIN_VERSION_template_haskell(2,8,0)
_NumTyLit :: Prism' TyLit Integer
_NumTyLit
  = prism remitter reviewer
  where
      remitter = NumTyLit
      reviewer (NumTyLit x) = Right x
      reviewer x = Left x

_StrTyLit :: Prism' TyLit String
_StrTyLit
  = prism remitter reviewer
  where
      remitter = StrTyLit
      reviewer (StrTyLit x) = Right x
      reviewer x = Left x
#endif

#if !MIN_VERSION_template_haskell(2,10,0)
_ClassP :: Prism' Pred (Name, [Type])
_ClassP
  = prism remitter reviewer
  where
      remitter (x, y) = ClassP x y
      reviewer (ClassP x y) = Right (x, y)
      reviewer x = Left x

_EqualP :: Prism' Pred (Type, Type)
_EqualP
  = prism remitter reviewer
  where
      remitter (x, y) = EqualP x y
      reviewer (EqualP x y) = Right (x, y)
      reviewer x = Left x
#endif

#if MIN_VERSION_template_haskell(2,9,0)
_NominalR :: Prism' Role ()
_NominalR
  = prism remitter reviewer
  where
      remitter () = NominalR
      reviewer NominalR = Right ()
      reviewer x = Left x

_RepresentationalR :: Prism' Role ()
_RepresentationalR
  = prism remitter reviewer
  where
      remitter () = RepresentationalR
      reviewer RepresentationalR = Right ()
      reviewer x = Left x

_PhantomR :: Prism' Role ()
_PhantomR
  = prism remitter reviewer
  where
      remitter () = PhantomR
      reviewer PhantomR = Right ()
      reviewer x = Left x

_InferR :: Prism' Role ()
_InferR
  = prism remitter reviewer
  where
      remitter () = InferR
      reviewer InferR = Right ()
      reviewer x = Left x
#endif
