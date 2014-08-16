{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.PrismTH
-- Copyright   :  (C) 2014 Edward Kmett, (C) 2014 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Control.Lens.Internal.PrismTH
  ( makePrisms
  , makeClassyPrisms
  , makeDecPrisms
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Internal.TH
import Control.Lens.Iso
import Control.Lens.Prism
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Monad
import Data.Set.Lens
import Data.Traversable (sequenceA)
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Generate a 'Prism' for each constructor of a data type.
-- Isos generated when possible.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makePrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- _Foo :: Prism' (FooBarBaz a) Int
-- _Bar :: Prism (FooBarBaz a) (FooBarBaz b) a b
-- _Baz :: Prism' (FooBarBaz a) (Int, Char)
-- @
makePrisms :: Name {- ^ Type constructor name -} -> DecsQ
makePrisms = makePrisms' True


-- | Generate a 'Prism' for each constructor of a data type
-- and combine them into a single class. No Isos are created.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makeClassyPrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- class AsFooBarBaz s a | s -> a where
--   _FooBarBaz :: Prism' s (FooBarBaz a)
--   _Foo :: Prism' s Int
--   _Bar :: Prism' s a
--   _Baz :: Prism' s (Int,Char)
--
--   _Foo = _FooBarBaz . _Foo
--   _Bar = _FooBarBaz . _Bar
--   _Baz = _FooBarBaz . _Baz
--
-- instance AsFooBarBaz (FooBarBaz a) a
-- @
-- | Generate an "As" class of prisms. Names are selected by prefixing the constructor
-- name with an underscore.  Constructors with multiple fields will
-- construct Prisms to tuples of those fields.
makeClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeClassyPrisms = makePrisms' False


-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: Bool -> Name -> DecsQ
makePrisms' normal typeName =
  do info <- reify typeName
     case info of
       TyConI dec -> makeDecPrisms normal dec
       _          -> fail "makePrisms: expected type constructor name"


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms normal dec = case dec of
  DataD        _ ty vars cons _ -> next ty (convertTVBs vars) cons
  NewtypeD     _ ty vars con  _ -> next ty (convertTVBs vars) [con]
  DataInstD    _ ty tys  cons _ -> next ty tys                cons
  NewtypeInstD _ ty tys  con  _ -> next ty tys                [con]
  _                             -> fail "makePrisms: expected type constructor dec"
  where
  convertTVBs = map (VarT . bndrName)

  next ty args cons =
    makeConsPrisms (conAppsT ty args) (map normalizeCon cons) cls
    where
    cls | normal    = Nothing
        | otherwise = Just ty


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: Type -> [(Name,[Type])] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
makeConsPrisms s [(con,types)] Nothing = makeConIso s con types

-- top-level definitions
makeConsPrisms s cons Nothing = concat <$> sequence
  [ makeConPrism (prismName conName) s cons conName | conName <- map fst cons ]

-- classy prism class and instance
makeConsPrisms s cons (Just typeName) =
  sequence
    [ makeClassyPrismClass s className methodName cons
    , makeClassyPrismInstance s className methodName cons
    ]
  where
  className = mkName ("As" ++ nameBase typeName)
  methodName = prismName typeName


-- | Compute the full type-changing Prism type given an outer type,
-- list of constructors, and target constructor name. Additionally
-- return 'True' if the resulting type is a "simple" prism.
computePrismType :: Type -> [(Name,[Type])] -> Name -> Q (TypeQ, Bool)
computePrismType s cons conName =
  do (ts, cons') <- extract conName cons
     let unbound = setOf typeVars s Set.\\ setOf typeVars (map snd cons')
         simp    = Set.null unbound
     sub <- sequenceA (Map.fromSet (newName . nameBase) unbound)
     let a  = toTupleT (map return ts)
         b  = toTupleT (map return (substTypeVars sub ts))
         s' = return s
         t  = return (substTypeVars sub s)

#ifndef HLINT
         ty | simp      = close =<< [t| Prism' $s'    $a    |]
            | otherwise = close =<< [t| Prism  $s' $t $a $b |]
#endif

     return (ty, simp)


-- | Construct the top-level declaration syntax for a prism given
-- a definition name, outer type, constructors, and target constructor.
--
-- defName :: sType
-- defName = prism <<reviewer>> <<remitter>>
makeConPrism :: Name -> Type -> [(Name,[Type])] -> Name -> DecsQ
makeConPrism defName s cons conName =
  do (ty, simp) <- computePrismType s cons conName
     sequence
       [ sigD defName ty
       , valD (varP defName) (normalB (makeConPrismExp cons conName simp)) []
       ]


computeIsoType :: Type -> [Type] -> TypeQ
computeIsoType s' fields =
  do sub <- sequenceA (Map.fromSet (newName . nameBase) (setOf typeVars s'))
     let s = return                    s'
         t = return (substTypeVars sub s')
         a = toTupleT (map return                    fields)
         b = toTupleT (map return (substTypeVars sub fields))

#ifndef HLINT
         ty | Map.null sub = [t| Iso' $s    $a    |]
            | otherwise    = [t| Iso  $s $t $a $b |]
#endif

     close =<< ty

makeConIso :: Type -> Name -> [Type] -> DecsQ
makeConIso s conName fields =
  do let ty      = computeIsoType s fields
         defName = prismName conName
     sequence
       [ sigD       defName  ty
       , valD (varP defName) (normalB (makeConIsoExp conName (length fields))) []
       ]


-- | Construct prism expression
--
-- prism <<reviewer>> <<remitter>>
makeConPrismExp ::
  [(Name,[Type])] {- ^ constructors       -} ->
  Name            {- ^ target constructor -} ->
  Bool            {- ^ make simple prisms -} ->
  ExpQ
makeConPrismExp cons conName simp = [| prism $reviewer $remitter |]
  where
  Just ts = lookup conName cons
  fields  = length ts
  cons'   = map (over _2 length) cons

  reviewer             = makeReviewer       conName fields
  remitter | simp      = makeSimpleRemitter conName fields
           | otherwise = makeFullRemitter cons' conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: Name -> Int -> ExpQ
makeConIsoExp conName fields = [| iso $remitter $reviewer |]
  where
  reviewer = makeReviewer    conName fields
  remitter = makeIsoRemitter conName fields


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
makeReviewer :: Name -> Int -> ExpQ
makeReviewer conName fields =
  do xs <- replicateM fields (newName "x")
     lam1E (toTupleP (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter :: Name -> Int -> ExpQ
makeSimpleRemitter conName fields =
  do x  <- newName "x"
     xs <- replicateM fields (newName "x")
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB [| Right $(toTupleE (map varE xs)) |])
                   []
           , match wildP (normalB [| Left $(varE x) |]) []
           ]
     lam1E (varP x) (caseE (varE x) matches)


-- | Pattern match all constructors to enable type-changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          Other_n w   -> Left (Other_n w)
-- ) :: s -> Either t a
makeFullRemitter :: [(Name,Int)] -> Name -> ExpQ
makeFullRemitter cons target =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (conName,n) =
    do xs <- replicateM n (newName "x")
       match (conP conName (map varP xs))
             (normalB
               (if conName == target
                  then [| Right $(toTupleE (map varE xs)) |]
                  else [| Left  $(conE conName `appsE1` map varE xs) |]))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
makeIsoRemitter :: Name -> Int -> ExpQ
makeIsoRemitter conName fields =
  do xs <- replicateM fields (newName "x")
     lam1E (conP conName (map varP xs))
           (toTupleE (map varE xs))


------------------------------------------------------------------------
-- Classy prisms
------------------------------------------------------------------------


-- | Construct the classy prisms class for a given type and constructors.
--
-- class ClassName r <<vars in type>> | r -> <<vars in Type>> where
--   topMethodName   :: Prism' r Type
--   conMethodName_n :: Prism' r conTypes_n
--   conMethodName_n = topMethodName . conMethodName_n
makeClassyPrismClass ::
  Type            {- Outer type      -} ->
  Name            {- Class name      -} ->
  Name            {- Top method name -} ->
  [(Name,[Type])] {- Constructors    -} ->
  DecQ
makeClassyPrismClass s className methodName cons =
  do r <- newName "r"
#ifndef HLINT
     let methodType = [t| Prism' $(varT r) $(return s) |]
#endif
     classD (cxt[]) className (map PlainTV (r : vs)) (fds r)
       ( sigD methodName methodType
       : concatMap (mkMethod (varT r)) cons'
       )

  where
#ifndef HLINT
  mkMethod r (defName,types) =
    [ sigD defName        [t| Prism' $r $(toTupleT (map return types)) |]
    , valD (varP defName) (normalB [| $(varE methodName) . $(varE defName) |]) []
    ]
#endif

  cons'         = map (over _1 prismName) cons
  vs            = Set.toList (setOf typeVars s)
  fds r
    | null vs   = []
    | otherwise = [FunDep [r] vs]



-- | Construct the classy prisms instance for a given type and constructors.
--
-- instance Classname OuterType where
--   topMethodName = id
--   conMethodName_n = <<prism>>
makeClassyPrismInstance ::
  Type            {- Outer type      -} ->
  Name            {- Class name      -} ->
  Name            {- Top method name -} ->
  [(Name,[Type])] {- Constructors    -} ->
  DecQ
makeClassyPrismInstance s className methodName cons =
  do let vs = Set.toList (setOf typeVars s)
         cls = className `conAppsT` (s : map VarT vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB [| id |]) []
       : [ valD (varP (prismName conName))
                (normalB (makeConPrismExp cons conName True)) []
           | conName <- map fst cons
           ]
       )


------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------


-- | Normalize 'Con' to its constructor name and field types.
normalizeCon :: Con -> (Name, [Type])
normalizeCon (RecC    conName xs) = (conName, map (view _3) xs)
normalizeCon (NormalC conName xs) = (conName, map (view _2) xs)
normalizeCon (InfixC (_,x) conName (_,y)) = (conName, [x,y])
normalizeCon ForallC{} = error "existentials not yet supported"


-- | Compute a prism's name by prefixing an underscore
prismName :: Name -> Name
prismName = mkName . ('_':) . nameBase


-- | Quantify all the free variables in a type.
close :: Type -> TypeQ
close t = forallT (map PlainTV (Set.toList vs)) (cxt[]) (return t)
  where
  vs = setOf typeVars t


-- | Lookup a value in an association list and return the association
-- list with that key-value pair removed.
extract :: Eq k => k -> [(k,v)] -> Q (v , [(k,v)])
extract _ [] = fail "extract: Unable to find constructor"
extract x ((k,v):kvs)
  | x == k = return (v, kvs)
  | otherwise = do (r,kvs') <- extract x kvs
                   return (r,(k,v):kvs')
