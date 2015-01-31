-- The paddles

module Pong.Paddle where

import Graphics.Gloss

import qualified Pong.Config as Config

-----------
-- TYPES --
-----------

data Paddle = Paddle {
  paddleIsPlayer :: Bool,
  paddleHeight :: Float
}

----------------------
-- PADDLE FUNCTIONS --
----------------------

-- The width of a paddle
paddleWidth :: Float
paddleWidth = 10

-- The size (height-radius-ish) of a paddle
paddleSize :: Float
paddleSize = 25

-- The maximum height of a paddle
maxHeight :: Float
maxHeight = fromIntegral Config.height / 2 - paddleSize

-- The minimum height of a paddle
minHeight :: Float
minHeight = (-maxHeight)

-- The player's paddle
playerPaddle :: Paddle
playerPaddle = makePaddle True 0

-- The computer's paddle
computerPaddle :: Paddle
computerPaddle = makePaddle False 0

-- Create a paddle at the given height
makePaddle :: Bool -> Float -> Paddle
makePaddle p h = Paddle {
  paddleIsPlayer = p,
  paddleHeight = h
}

-- Move a paddle the given distance
movePaddle :: Paddle -> Float -> Paddle
movePaddle p@(Paddle { paddleHeight = h }) d
  | (h >= maxHeight && d > 0) || (h <= minHeight && d < 0) = p
  | otherwise = p { paddleHeight = h + d }

-- Draw a paddle rectangle
paddleRect :: Picture
paddleRect = color Config.pongGreen $ polygon [
  (0,-paddleSize),
  (paddleWidth,-paddleSize),
  (paddleWidth,paddleSize),
  (0,paddleSize)
  ]

-- Render a paddle
renderPaddle :: Paddle -> Picture
renderPaddle Paddle { paddleIsPlayer = p, paddleHeight = y } =
  let dx = (fromIntegral Config.width / if p then -2.0 else 2.0) - if p then 0.0 else paddleWidth
  in translate dx y $ paddleRect
