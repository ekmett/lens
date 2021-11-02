{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2012 Edward Kmett, Niklas Haas
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  TH, Rank2, NoMonomorphismRestriction
--
-- A simple game of pong using gloss.
-----------------------------------------------------------------------------
module Main where

import Control.Lens hiding ((:>), at)
import Control.Monad.State (State, execState, get)
import Control.Monad (when)

import Data.Set (Set, empty)
import Data.Stream.Infinite (Stream(..))

import Graphics.Gloss hiding (display)
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)

-- Some global constants

gameSize :: Float
gameSize        = 300

windowWidth, windowHeight :: Int
windowWidth     = 800
windowHeight    = 600

ballRadius, speedIncrease, losingAccuracy, winningAccuracy,
  initialSpeed, paddleWidth, paddleHeight, paddleSpeed :: Float
ballRadius      = 0.02
speedIncrease   = 1.2
losingAccuracy  = 0.9
winningAccuracy = 0.1
initialSpeed    = 0.6
paddleWidth     = 0.02
paddleHeight    = 0.3
paddleSpeed     = 1

textSize :: Float
textSize        = 0.001

-- Pure data type for representing the game state

data Pong = Pong
  { _ballPos   :: Point
  , _ballSpeed :: Vector
  , _paddle1   :: Float
  , _paddle2   :: Float
  , _score     :: (Int, Int)
  , _vectors   :: Stream Vector

  -- Since gloss doesn't cover this, we store the set of pressed keys
  , _keys      :: Set Key
  }

-- Some nice lenses to go with it
makeLenses ''Pong

-- Renamed tuple lenses for enhanced clarity with points/vectors
_x :: Field1 s t a b => Lens s t a b
_x = _1

_y :: Field2 s t a b => Lens s t a b
_y = _2

initial :: Pong
initial = Pong (0, 0) (0, 0) 0 0 (0, 0) (return (0, 0)) empty

-- Calculate the y position at which the ball will next hit (on player2's side)
hitPos :: Point -> Vector -> Float
hitPos (x,y) (u,v) = ypos
  where
    xdist = if u >= 0 then 1 - x else 3 + x
    time  = xdist / abs u
    ydist = v * time
    ypos  = bounce (y + ydist)
    o     = 1 - ballRadius

    -- Calculate bounces iteratively
    bounce n
      | n >  o    = bounce (  2 *o - n)
      | n < -o    = bounce ((-2)*o - n)
      | otherwise = n

-- Difficulty function
accuracy :: Pong -> Float
accuracy p = g . f . fromIntegral $ p^.score._1 - p^.score._2
  where
    -- Scaling function
    f x = 0.04 * x + 0.5
    -- Clamping function
    g = min losingAccuracy . max winningAccuracy

-- Game update logic

update :: Float -> Pong -> Pong
update time = execState $ do
  updatePaddles time
  updateBall time
  checkBounds

-- Move the ball by adding its current speed
updateBall :: Float -> State Pong ()
updateBall time = do
  (u, v) <- use ballSpeed
  ballPos %= (Pt.+ (time * u, time * v))

  -- Make sure it doesn't leave the playing area
  ballPos.both %= clamp ballRadius

-- Update the paddles
updatePaddles :: Float -> State Pong ()
updatePaddles time = do
  p <- get

  let paddleMovement = time * paddleSpeed
      keyPressed key = p^.keys.contains (SpecialKey key)

  -- Update the player's paddle based on keys
  when (keyPressed KeyUp)   $ paddle1 += paddleMovement
  when (keyPressed KeyDown) $ paddle1 -= paddleMovement

  -- Calculate the optimal position
  let optimal = hitPos (p^.ballPos) (p^.ballSpeed)
      acc     = accuracy p
      target  = optimal * acc + (p^.ballPos._y) * (1 - acc)
      dist    = target - p^.paddle2

  -- Move the CPU's paddle towards this optimal position as needed
  when (abs dist > paddleHeight/3) $
    case compare dist 0 of
      GT -> paddle2 += paddleMovement
      LT -> paddle2 -= paddleMovement
      _  -> return ()

  -- Make sure both paddles don't leave the playing area
  paddle1 %= clamp (paddleHeight/2)
  paddle2 %= clamp (paddleHeight/2)

-- Clamp to the region (-1, 1) but with padding
clamp :: Float -> Float -> Float
clamp pad = max (pad - 1) . min (1 - pad)

-- Check for collisions and/or scores
checkBounds :: State Pong ()
checkBounds = do
  p <- get
  let (x,y) = p^.ballPos

  -- Check for collisions with the top or bottom
  when (abs y >= edge) $
    ballSpeed._y %= negate

  -- Check for collisions with paddles
  let check paddle other
        | y >= p^.paddle - paddleHeight/2 && y <= p^.paddle + paddleHeight/2 = do
            ballSpeed._x   %= negate
            ballSpeed._y   += 3*(y - p^.paddle) -- add english
            ballSpeed.both *= speedIncrease
        | otherwise = do
          score.other += 1
          reset

  when (x >=  edge) $ check paddle2 _1
  when (x <= -edge) $ check paddle1 _2

  where
    edge = 1 - ballRadius

-- Reset the game
reset :: State Pong ()
reset = do
  ballPos .= (0, 0)
  ballSpeed <~ nextSpeed

-- Retrieve a speed from the list, dropping it in the process
nextSpeed :: State Pong Vector
nextSpeed = do
  v:>vs <- use vectors
  vectors .= vs
  return v

-- Drawing a pong state to the screen

draw :: Pong -> Picture
draw p = scale gameSize gameSize $ Pictures
  [ drawBall   `at` p^.ballPos
  , drawPaddle `at` (-paddleX, p^.paddle1)
  , drawPaddle `at` ( paddleX, p^.paddle2)

  -- Score and playing field
  , drawScore (p^.score) `at` (-0.1, 0.85)
  , rectangleWire 2 2
  ]
  where
    paddleX = 1 + paddleWidth/2
    po `at` (x,y) = translate x y po; infixr 1 `at`

drawPaddle :: Picture
drawPaddle = rectangleSolid paddleWidth paddleHeight

drawBall :: Picture
drawBall = circleSolid ballRadius

drawScore :: (Int, Int) -> Picture
drawScore (x, y) = scale textSize textSize . text $ show x ++ " " ++ show y

-- Handle input by simply updating the keys set

handle :: Event -> Pong -> Pong
handle (EventKey k s _ _) = keys.contains k .~ (s == Down)
handle _ = id

-- The main program action

main :: IO ()
main = do
  v:>vs <- startingSpeeds
  let world = ballSpeed .~ v $ vectors .~ vs $ initial
  play display backColor fps world draw handle update

  where
    display   = InWindow "Pong!" (windowWidth, windowHeight) (200, 200)
    backColor = white
    fps       = 120

-- Generate the random list of starting speeds

startingSpeeds :: IO (Stream Vector)
startingSpeeds = do
  rs <- randomRs (-initialSpeed, initialSpeed) <$> newStdGen
  return . listToStream . interleave $ filter ((> 0.2) . abs) rs

  where
    interleave :: [a] -> [(a,a)]
    interleave (x:y:xs) = (x,y) : interleave xs
    interleave _        = []

    -- Assumes the list is infinite.
    listToStream :: [a] -> Stream a
    listToStream = foldr (:>) (error "Finite list")
