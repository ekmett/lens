{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | A simple Turtle-graphics demonstration for modeling the location of a turtle.
--
-- This is based on the code presented by Seth Tisue at the Boston Area Scala
-- Enthusiasts meeting during his lens talk.
--
-- Usage:
--
-- > def & forward 10 & down & color .~ red % turn (pi/2) & forward 5
module Turtle where

import Control.Lens
import Data.Default

data Point = Point
  { __x, __y :: Double
  } deriving (Eq,Show)

makeClassy ''Point

instance Default Point where
  def = Point def def

data Color = Color
  { __r, __g, __b :: Int
  } deriving (Eq,Show)

makeClassy ''Color

red :: Color
red = Color 255 0 0

instance Default Color where
  def = Color def def def

data Turtle = Turtle
  { _tPoint  :: Point
  , _tColor  :: Color
  , _heading :: Double
  , _penDown :: Bool
  } deriving (Eq,Show)

makeClassy ''Turtle

instance Default Turtle where
  def = Turtle def def def False

instance HasPoint Turtle where
  point = tPoint

instance HasColor Turtle where
  color = tColor

forward :: Double -> Turtle -> Turtle
forward d t =
  t & _y +~ d * cos (t^.heading)
    & _x +~ d * sin (t^.heading)

turn :: Double -> Turtle -> Turtle
turn d = heading +~ d

up, down :: Turtle -> Turtle
up   = penDown .~ False
down = penDown .~ True
