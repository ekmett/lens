{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hunit)
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a simple hunit test suite for lens.
--
-- The code attempts to enumerate common use cases rather than give an example
-- of each available lens function. The tests here merely scratch the surface
-- of what is possible using the lens package; there are a great many use cases
-- (and lens functions) that aren't covered.
-----------------------------------------------------------------------------
module Main (main) where

import Control.Lens
import Control.Monad.State
import Data.Char
import qualified Data.Text as StrictT
import qualified Data.Text.Lazy as LazyT
import qualified Data.ByteString as StrictB
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test)


data Point =
  Point
  { _x :: Int -- ^ X coordinate
  , _y :: Int -- ^ Y coordinate
  } deriving (Show, Eq, Ord)

makeLenses ''Point

data Box =
  Box
  { _low :: Point  -- ^ The lowest used coordinates.
  , _high :: Point -- ^ The highest used coordinates.
  } deriving (Show, Eq)

makeLenses ''Box

data Polygon =
  Polygon
  { _points :: [ Point ]
  , _labels :: Map Point String
  , _box :: Box
  } deriving (Show, Eq)

makeLenses ''Polygon

data Shape = SBox Box | SPolygon Polygon | SCircle Point Int | SVoid
makePrisms ''Shape

origin =
  Point { _x = 0, _y = 0 }

vectorFrom fromPoint toPoint =
  Point
  { _x = toPoint^.x - fromPoint^.x
  , _y = toPoint^.y - fromPoint^.y
  }

trig =
  Polygon
  { _points = [ Point { _x = 0, _y = 0 }
              , Point { _x = 4, _y = 7 }
              , Point { _x = 8, _y = 0 } ]
  , _labels = Map.fromList [ (Point { _x = 0, _y = 0 }, "Origin")
                           , (Point { _x = 4, _y = 7 }, "Peak") ]
  , _box = Box { _low = Point { _x = 0, _y = 0 }
               , _high = Point { _x = 8, _y = 7 } }
  }

case_read_record_field =
  (trig^.box.high.y)
    @?= 7

case_read_state_record_field =
  runState test trig @?= (7, trig)
  where
    test = use $ box.high.y

case_read_record_field_and_apply_function =
  (trig^.points.to last.to (vectorFrom origin).x)
    @?= 8

case_read_state_record_field_and_apply_function =
  runState test trig @?= (8, trig)
  where test = use $ points.to last.to (vectorFrom origin).x

case_write_record_field =
  (trig & box.high.y .~ 6)
    @?= trig { _box = (trig & _box)
               { _high = (trig & _box & _high)
                         { _y = 6 } } }

case_write_state_record_field = do
  let trig' = trig { _box = (trig & _box)
                            { _high = (trig & _box & _high)
                                      { _y = 6 } } }
  runState test trig @?= ((), trig')
  where
    test = box.high.y .= 6

case_write_record_field_and_access_new_value =
  (trig & box.high.y <.~ 6)
    @?= (6, trig { _box = (trig & _box)
                          { _high = (trig & _box & _high)
                                    { _y = 6 } } })

case_write_state_record_field_and_access_new_value = do
  let trig' = trig { _box = (trig & _box)
                            { _high = (trig & _box & _high)
                                      { _y = 6 } } }
  runState test trig @?= (6, trig')
  where
    test = box.high.y <.= 6

case_write_record_field_and_access_old_value =
  (trig & box.high.y <<.~ 6)
    @?= (7, trig { _box = (trig & _box)
                          { _high = (trig & _box & _high)
                                    { _y = 6 } } })

case_write_state_record_field_and_access_old_value = do
  let trig' = trig { _box = (trig & _box)
                            { _high = (trig & _box & _high)
                                      { _y = 6 } } }
  runState test trig @?= (7, trig')
  where
    test = box.high.y <<.= 6

case_modify_record_field =
  (trig & box.low.y %~ (+ 2))
    @?= trig { _box = (trig & _box)
                      { _low = (trig & _box & _low)
                               { _y = ((trig & _box & _low & _y) + 2) } } }

case_modify_state_record_field = do
  let trig' = trig { _box = (trig & _box)
                            { _low = (trig & _box & _low)
                                     { _y = ((trig & _box & _low & _y) + 2) } } }
  runState test trig @?= ((), trig')
  where
    test = box.low.y %= (+ 2)

case_modify_record_field_and_access_new_value =
  (trig & box.low.y <%~ (+ 2))
    @?= (2, trig { _box = (trig & _box)
                          { _low = (trig & _box & _low)
                                   { _y = ((trig & _box & _low & _y) + 2) } } })

case_modify_state_record_field_and_access_new_value = do
  let trig' = trig { _box = (trig & _box)
                            { _low = (trig & _box & _low)
                                     { _y = ((trig & _box & _low & _y) + 2) } } }
  runState test trig @?= (2, trig')
  where
    test = box.low.y <%= (+ 2)

case_modify_record_field_and_access_old_value =
  (trig & box.low.y <<%~ (+ 2))
    @?= (0, trig { _box = (trig & _box)
                          { _low = (trig & _box & _low)
                                   { _y = ((trig & _box & _low & _y) + 2) } } })

case_modify_state_record_field_and_access_old_value = do
  let trig' = trig { _box = (trig & _box)
                            { _low = (trig & _box & _low)
                                     { _y = ((trig & _box & _low & _y) + 2) } } }
  runState test trig @?= (0, trig')
  where
    test = box.low.y <<%= (+ 2)

case_modify_record_field_and_access_side_result = do
  runState test trig @?= (8, trig')
  where
    test = box.high %%= modifyAndCompute
    modifyAndCompute point =
      (point ^. x, point & y +~ 2)
    trig' = trig { _box = (trig & _box)
                            { _high = (trig & _box & _high)
                                      { _y = ((trig & _box & _high & _y) + 2) } } }

case_increment_record_field =
  (trig & box.low.y +~ 1) -- And similarly for -~ *~ //~ ^~ ^^~ **~ ||~ &&~
    @?= trig { _box = (trig & _box)
                      { _low = (trig & _box & _low)
                               { _y = ((trig & _box & _low & _y) + 1) } } }

case_increment_state_record_field =
  runState test trig @?= ((), trig')
  where
    test = box.low.y += 1
    trig' = trig { _box = (trig & _box)
                   { _low = (trig & _box & _low)
                     { _y = ((trig & _box & _low & _y) + 1) } } }

case_append_to_record_field =
  (trig & points <>~ [ origin ])
    @?= trig { _points = (trig & _points) <> [ origin ] }

case_append_to_state_record_field = do
  runState test trig @?= ((), trig')
  where
    test = points <>= [ origin ]
    trig' = trig { _points = (trig & _points) <> [ origin ] }

case_append_to_record_field_and_access_new_value =
  (trig & points <<>~ [ origin ])
    @?= (_points trig <> [ origin ], trig { _points = (trig & _points) <> [ origin ] })

case_append_to_state_record_field_and_access_new_value = do
  runState test trig @?= (_points trig <> [ origin ], trig')
  where
    test = points <<>= [ origin ]
    trig' = trig { _points = (trig & _points) <> [ origin ] }

case_prepend_to_record_field =
  (trig & points <>:~ [ origin ])
    @?= trig { _points = [ origin ] <> (trig & _points) }

case_prepend_to_state_record_field = do
  runState test trig @?= ((), trig')
  where
    test = points <>:= [ origin ]
    trig' = trig { _points = [ origin ] <> (trig & _points) }

case_prepend_to_record_field_and_access_new_value =
  (trig & points <<>:~ [ origin ])
    @?= ([ origin ] <> _points trig, trig { _points = [ origin ] <> (trig & _points) })

case_prepend_to_state_record_field_and_access_new_value = do
  runState test trig @?= ([ origin ] <> _points trig, trig')
  where
    test = points <<>:= [ origin ]
    trig' = trig { _points = [ origin ] <> (trig & _points) }

case_cons_to_record_field =
  (trig & points <|~ origin)
    @?= trig { _points = origin : (trig & _points) }

case_cons_to_state_record_field = do
  runState test trig @?= ((), trig')
  where
    test = points <|= origin
    trig' = trig { _points = origin : (trig & _points) }

case_cons_to_record_field_and_access_new_value =
  (trig & points <<|~ origin)
    @?= (origin : _points trig, trig { _points = origin : (trig & _points) })

case_cons_to_state_record_field_and_access_new_value =
  runState test trig @?= ([ origin ] <> _points trig, trig')
  where
    test = points <<|= origin
    trig' = trig { _points = origin : (trig & _points) }

case_snoc_to_record_field =
  (trig & points |>~ origin)
    @?= trig { _points = (trig & _points) `snoc` origin }

case_snoc_to_state_record_field = do
  runState test trig @?= ((), trig')
  where
    test = points |>= origin
    trig' = trig { _points = (trig & _points) `snoc` origin }

case_snoc_to_record_field_and_access_new_value =
  (trig & points <|>~ origin)
    @?= (_points trig `snoc` origin, trig { _points = (trig & _points) `snoc` origin })

case_snoc_to_state_record_field_and_access_new_value =
  runState test trig @?= (_points trig <> [ origin ], trig')
  where
    test = points <|>= origin
    trig' = trig { _points = (trig & _points) `snoc` origin }

case_append_to_record_field_and_access_old_value =
  (trig & points <<%~ (<>[origin]))
    @?= (_points trig, trig { _points = (trig & _points) <> [ origin ] })

case_append_to_state_record_field_and_access_old_value = do
  runState test trig @?= (_points trig, trig')
  where
    test = points <<%= (<>[origin])
    trig' = trig { _points = (trig & _points) <> [ origin ] }

case_read_maybe_map_entry = trig^.labels.at origin @?= Just "Origin"

case_read_maybe_state_map_entry =
  runState test trig @?= (Just "Origin", trig)
  where test = use $ labels.at origin

case_read_map_entry = trig^.labels.ix origin @?= "Origin"

case_read_state_map_entry = runState test trig @?= ("Origin", trig)
  where test = use $ labels.ix origin

case_modify_map_entry =
  (trig & labels.ix origin %~ List.map toUpper)
    @?= trig { _labels = Map.fromList [ (Point { _x = 0, _y = 0 }, "ORIGIN")
                                      , (Point { _x = 4, _y = 7 }, "Peak") ] }

case_insert_maybe_map_entry =
  (trig & labels.at (Point { _x = 8, _y = 0 }) .~ Just "Right")
    @?= trig { _labels = Map.fromList [ (Point { _x = 0, _y = 0 }, "Origin")
                                      , (Point { _x = 4, _y = 7 }, "Peak")
                                      , (Point { _x = 8, _y = 0 }, "Right") ] }

case_delete_maybe_map_entry =
  (trig & labels.at origin .~ Nothing)
    @?= trig { _labels = Map.fromList [ (Point { _x = 4, _y = 7 }, "Peak") ] }

case_read_list_entry =
  (trig ^? points.element 0)
    @?= Just origin

case_write_list_entry =
  (trig & points.element 0 .~ Point { _x = 2, _y = 0 })
    @?= trig { _points = [ Point { _x = 2, _y = 0 }
                         , Point { _x = 4, _y = 7 }
                         , Point { _x = 8, _y = 0 } ] }

case_write_through_list_entry =
  (trig & points.element 0 . x .~ 2)
    @?= trig { _points = [ Point { _x = 2, _y = 0 }
                         , Point { _x = 4, _y = 7 }
                         , Point { _x = 8, _y = 0 } ] }

case_correct_indexing_strict_text =
  map (\i -> StrictT.pack "12" ^? ix i) [-1..2]
    @?= [Nothing, Just '1', Just '2', Nothing]

case_correct_indexing_lazy_text =
  map (\i -> LazyT.pack "12" ^? ix i) [-1..2]
    @?= [Nothing, Just '1', Just '2', Nothing]

case_correct_indexing_strict_bytestring =
  map (\i -> StrictB.pack [1,2] ^? ix i) [-1..2]
    @?= [Nothing, Just 1, Just 2, Nothing]

case_correct_indexing_lazy_bytestring =
  map (\i -> LazyB.pack [1,2] ^? ix i) [-1..2]
    @?= [Nothing, Just 1, Just 2, Nothing]

main :: IO ()
main = defaultMain
  [ testGroup "Main"
    [ testCase "read record field" case_read_record_field
    , testCase "read state record field" case_read_state_record_field
    , testCase "read record field and apply function" case_read_record_field_and_apply_function
    , testCase "read state record field and apply function" case_read_state_record_field_and_apply_function
    , testCase "write record field" case_write_record_field
    , testCase "write state record field" case_write_state_record_field
    , testCase "write record field and access new value" case_write_record_field_and_access_new_value
    , testCase "write state record field and access new value" case_write_state_record_field_and_access_new_value
    , testCase "write record field and access old value" case_write_record_field_and_access_old_value
    , testCase "write state record field and access old value" case_write_state_record_field_and_access_old_value
    , testCase "modify record field" case_modify_record_field
    , testCase "modify state record field" case_modify_state_record_field
    , testCase "modify record field and access new value" case_modify_record_field_and_access_new_value
    , testCase "modify state record field and access new value" case_modify_state_record_field_and_access_new_value
    , testCase "modify record field and access old value" case_modify_record_field_and_access_old_value
    , testCase "modify state record field and access old value" case_modify_state_record_field_and_access_old_value
    , testCase "modify record field and access side result" case_modify_record_field_and_access_side_result
    , testCase "increment record field" case_increment_record_field
    , testCase "increment state record field" case_increment_state_record_field
    , testCase "append to record field" case_append_to_record_field
    , testCase "append to state record field" case_append_to_state_record_field
    , testCase "prepend to record field" case_prepend_to_record_field
    , testCase "prepend to state record field" case_prepend_to_state_record_field
    , testCase "cons to record field" case_cons_to_record_field
    , testCase "cons to state record field" case_cons_to_state_record_field
    , testCase "snoc to record field" case_snoc_to_record_field
    , testCase "snoc to state record field" case_snoc_to_state_record_field
    , testCase "append to record field and access new value" case_append_to_record_field_and_access_new_value
    , testCase "append to state record field and access new value" case_append_to_state_record_field_and_access_new_value
    , testCase "prepend to record field and access new value" case_prepend_to_record_field_and_access_new_value
    , testCase "prepend to state record field and access new value" case_prepend_to_state_record_field_and_access_new_value
    , testCase "cons to record field and access new value" case_cons_to_record_field_and_access_new_value
    , testCase "cons to state record field and access new value" case_cons_to_state_record_field_and_access_new_value
    , testCase "snoc to record field and access new value" case_snoc_to_record_field_and_access_new_value
    , testCase "snoc to state record field and access new value" case_snoc_to_state_record_field_and_access_new_value
    , testCase "append to record field and access old value" case_append_to_record_field_and_access_old_value
    , testCase "append to state record field and access old value" case_append_to_state_record_field_and_access_old_value
    , testCase "read maybe map entry" case_read_maybe_map_entry
    , testCase "read maybe state map entry" case_read_maybe_state_map_entry
    , testCase "read map entry" case_read_map_entry
    , testCase "read state map entry" case_read_state_map_entry
    , testCase "modify map entry" case_modify_map_entry
    , testCase "insert maybe map entry" case_insert_maybe_map_entry
    , testCase "delete maybe map entry" case_delete_maybe_map_entry
    , testCase "read list entry" case_read_list_entry
    , testCase "write list entry" case_write_list_entry
    , testCase "write through list entry" case_write_through_list_entry
    , testCase "correct indexing strict text" case_correct_indexing_strict_text
    , testCase "correct indexing lazy text" case_correct_indexing_lazy_text
    , testCase "correct indexing strict bytestring" case_correct_indexing_strict_bytestring
    , testCase "correct indexing lazy bytestring" case_correct_indexing_lazy_bytestring
    ]
  ]
