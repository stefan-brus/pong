-- The ball

module Pong.Ball where

import Graphics.Gloss

import qualified Pong.Config as Config

-----------
-- TYPES --
-----------

data Ball = Ball {
  ballLocation :: (Float,Float)
}

--------------------
-- BALL FUNCTIONS --
--------------------

-- The size of the ball
ballSize :: Float
ballSize = 10

-- Create a ball with the given location
makeBall :: Float -> Float -> Ball
makeBall x y = Ball { ballLocation = (x,y) }

-- Render a ball
renderBall :: Ball -> Picture
renderBall (Ball { ballLocation=(x,y) }) = translate x y $ color Config.pongGreen $ thickCircle 2.5 (ballSize / 2)
