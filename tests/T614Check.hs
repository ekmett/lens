{-# LANGUAGE TemplateHaskell #-}
-- | Compile-time assertions that the optics from "T614" carry the right
-- documentation; a separate module because getDoc cannot see docs attached
-- by putDoc in the module being compiled. Helpers live inside the splice
-- (stage restriction); isInfixOf tolerates doc-string whitespace
-- differences across GHC versions.
module T614Check where

import Data.List (isInfixOf)
import Language.Haskell.TH
import T614

$(do let expectDoc expected n = do
           mdoc <- getDoc (DeclDoc n)
           case mdoc of
             Just doc | expected `isInfixOf` doc -> return ()
             _ -> fail $ "T614Check: expected documentation containing "
                      ++ show expected ++ " on " ++ show n
                      ++ ", got " ++ show mdoc
         expectNoDocLike banned n = do
           mdoc <- getDoc (DeclDoc n)
           case mdoc of
             Just doc | banned `isInfixOf` doc ->
               fail $ "T614Check: documentation of " ++ show n
                   ++ " wrongly contains " ++ show banned
                   ++ ": " ++ show mdoc
             _ -> return ()
         expectNoDoc n = do
           mdoc <- getDoc (DeclDoc n)
           case mdoc of
             Nothing -> return ()
             Just doc -> fail $ "T614Check: expected no documentation on "
                             ++ show n ++ ", got " ++ show doc

     expectDoc "Weight in grams." 'melonWeight              -- makeLenses
     expectNoDoc 'melonUndocumented                         -- no invented docs
     expectDoc "Number of bunks." 'cabinBunks               -- makeClassy
     expectDoc "Whether the cabin is heated." 'cabinHeat
     expectNoDoc 'area                                      -- makeFields: shared class
     expectNoDoc 'twins                                     -- merged fields
     expectDoc "A circle with a radius." '_Circle           -- makePrisms
     expectDoc "A rectangle." '_Rect
     expectNoDoc '_UndocShape
     expectDoc "A temperature in Celsius." '_MkTemp         -- iso case
     expectDoc "Stop." '_Red                                -- makeClassyPrisms
     expectDoc "Go." '_Green
     expectDoc "Handwritten documentation that must survive." 'pearRipeness
     expectNoDocLike "Field documentation that must not win." 'pearRipeness
     return [])
