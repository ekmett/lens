{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State

import Data.Pair.Lens
import Data.Set (Set, member, empty, insert, delete)
import Data.Set.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Some global constants

gameSize      = 300
windowSize    = 480
ballRadius    = 0.02
initialSpeed  = (0.8, 0.3)
speedIncrease = 1.1
paddleWidth   = 0.02
paddleHeight  = 0.3
paddleSpeed   = 1
textSize      = 0.3 / gameSize

-- Pure data type for representing the game state

data Pong = Pong
  { _ballPos   :: Point
  , _ballSpeed :: Vector
  , _paddle1   :: Float
  , _paddle2   :: Float
  , _score     :: (Int, Int)

  -- Since gloss doesn't cover this, we store the set of pressed keys
  , _keys      :: Set Key
  }

initial :: Pong
initial = Pong (0, 0) initialSpeed 0 0 (0, 0) empty

-- Some nice lenses to go with it
makeLenses ''Pong

-- I'm just renaming the tuple lenses for enhanced clarity with points/vectors
_x = _1
_y = _2

-- Game update logic

update :: Float -> Pong -> Pong
update time = execState $ do
  updatePaddles time
  updateBall time
  checkBounds

-- Move the ball by adding its current speed
updateBall :: Float -> State Pong ()
updateBall time = do
  speed <- use ballSpeed
  ballPos += speed `mul` time

  -- Make sure it doesn't leave the playing area
  ballPos.both %= clampPad ballRadius

 where
   (a,b) `mul` c = (c*a, c*b)
   infixl 7 `mul`

-- Update the paddles
updatePaddles :: Float -> State Pong ()
updatePaddles time = do
  p <- get

  -- Update the player's paddle based on keys
  when (p^.keys.contains (SpecialKey KeyUp)) $
    paddle1 += paddleSpeed * time

  when (p^.keys.contains (SpecialKey KeyDown)) $
    paddle1 -= paddleSpeed * time

  -- Update the CPU's paddle based on the ball's relative position
  case compare (p^.ballPos._y) (p^.paddle2) of
    GT -> paddle2 += paddleSpeed * time
    LT -> paddle2 -= paddleSpeed * time
    _  -> return ()

  -- Make sure both paddles don't leave the playing area
  paddle1 %= clamp
  paddle2 %= clamp

 where
   clamp = clampPad (paddleHeight/2)

-- Clamp to the region (-1, 1) but with padding
clampPad :: Float -> Float -> Float
clampPad pad = max (pad - 1) . min (1 - pad)

-- Check for collisions and/or scores
checkBounds :: State Pong ()
checkBounds = do
  p <- get
  let (x,y) = p^.ballPos

  -- Check for collisions with the top or bottom
  when (abs y >= top) $
    ballSpeed._y %= negate

  -- Check for collisions with paddles
  let check l = y >= p^.l - paddleHeight/2 && y <= p^.l + paddleHeight/2
      collide = do
        ballSpeed._x   %= negate
        ballSpeed.both *= speedIncrease

  when (x <= left) $
    if check paddle1
      then collide
      else do
        score._2 += 1
        reset

  when (x >= right) $
    if check paddle2
      then collide
      else do
        score._1 += 1
        reset

  where
    top   = 1 - ballRadius
    left  = ballRadius + paddleWidth/2 - 1
    right = -left

-- Reset the game
reset :: State Pong ()
reset = do
  ballPos   .= initial^.ballPos
  ballSpeed .= initial^.ballSpeed

-- Drawing a pong state to the screen

draw :: Pong -> Picture
draw p = scale gameSize gameSize $ Pictures
  [ drawBall   `at` p^.ballPos
  , drawPaddle `at` (-1, p^.paddle1)
  , drawPaddle `at` ( 1, p^.paddle2)

  -- Score and playing field
  , scale textSize textSize (p^.score.pretty.to text) `at` (-0.1, 0.85)
  , rectangleWire 2 2
  ]
  where
    pretty = to (\(x,y) -> show x ++ " " ++ show y)
    p `at` (x,y) = translate x y p
    infixr 1 `at`

drawPaddle :: Picture
drawPaddle = rectangleSolid paddleWidth paddleHeight

drawBall :: Picture
drawBall = circleSolid ballRadius

-- Handle input by simply updating the keys set

handle :: Event -> Pong -> Pong
handle (EventKey k s _ _) = keys.contains k .~ (s == Down)
handle _ = id

-- The main program action

main = play display backColor fps initial draw handle update
  where
    -- display   = InWindow "Pong!" (windowSize, windowSize) (800, 600)
    display   = FullScreen (800,600)
    backColor = white
    fps       = 120
