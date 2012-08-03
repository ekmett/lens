{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Lens.TH
import Control.Monad.State

import Data.Set (Set, member, empty, insert, delete)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Some global constants

gameSize      = 300
windowSize    = 480
ballRadius    = 0.02
initialSpeed  = (0.5, 0.3)
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

-- This will be in Data.Pair.Lens soon
both :: Traversal (a,a) (b,b) a b
both f (x,y) = (,) <$> f x <*> f y

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
  when (SpecialKey KeyUp `isIn` p^.keys) $
    paddle1 += paddleSpeed * time

  when (SpecialKey KeyDown `isIn` p^.keys) $
    paddle1 -= paddleSpeed * time

  -- Calculate the optimal position
  let optimal = hitPos (p^.ballPos) (p^.ballSpeed)

  -- Move the CPU's paddle towards this optimal position
  case compare optimal (p^.paddle2) of
    GT -> paddle2 += paddleSpeed * time
    LT -> paddle2 -= paddleSpeed * time
    _  -> return ()

  -- Make sure both paddles don't leave the playing area
  paddle1 %= clamp
  paddle2 %= clamp

 where
   clamp = clampPad (paddleHeight/2)
   isIn  = member
   infixl 7 `isIn`

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

  let { collide = do
    ballSpeed._x   %= negate
    ballSpeed.both *= speedIncrease
  }

  when (x <= left) $
    if check paddle1
      then collide
      else score._2 += 1 >> reset

  when (x >= right) $
    if check paddle2
      then collide
      else score._1 += 1 >> reset

  where
    top   = 1 - ballRadius
    left  = ballRadius + paddleWidth/2 - 1
    right = -left

-- Reset the game
reset :: State Pong ()
reset = do
  ballPos   ^= initial^.ballPos
  ballSpeed ^= initial^.ballSpeed

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
    -- Pretty printing lens
    pretty = to (\(x,y) -> show x ++ " " ++ show y)
    p `at` (x,y) = translate x y p
    infixr 1 `at`

drawPaddle :: Picture
drawPaddle = rectangleSolid paddleWidth paddleHeight

drawBall :: Picture
drawBall = circleSolid ballRadius

-- Handle input by simply updating the keys set

handle :: Event -> Pong -> Pong
handle (EventKey k Down _ _) = keys %~ insert k
handle (EventKey k Up   _ _) = keys %~ delete k
handle _ = id

-- The main program action

main = play display backColor fps initial draw handle update
  where
    -- display   = InWindow "Pong!" (windowSize, windowSize) (200, 200)
    display = FullScreen (800,600)
    backColor = white
    fps       = 120
