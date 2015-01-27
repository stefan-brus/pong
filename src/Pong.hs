-- Main game module

module Pong where

import Data.Monoid

import Graphics.Gloss.Interface.Pure.Game

import qualified Pong.Config as Config
import Pong.Ball
import Pong.Paddle

-----------
-- TYPES --
-----------

-- The game state
data State = StateInit | StateGame | StateEnd

-- The current direction of the ball
data Direction = ToPlayer | ToComputer deriving (Eq)

-- The world
-- Speed is in pixels per second
data World = World {
  worldState :: State,
  worldPlayer :: Paddle,
  worldComputer :: Paddle,
  worldBall :: Ball,
  worldDir :: Direction,
  worldSpeed :: Float,
  worldAngle :: Float
}

--------------------------
-- GAME LOGIC FUNCTIONS --
--------------------------

-- Set up the initial state of the world
initWorld :: World
initWorld = World {
  worldState = StateInit,
  worldPlayer = playerPaddle,
  worldComputer = computerPaddle,
  worldBall = makeBall 0 0,
  worldDir = ToPlayer,
  worldSpeed = 100,
  worldAngle = 3.14159
}

-- Render the game given the state of the world
render :: World -> Picture
render World { worldState = StateInit } = renderInit
render World { worldState = StateEnd, worldDir = dir } = renderEnd dir
render w@(World { worldState = StateGame }) = renderGame w

-- Render the game
renderGame :: World -> Picture
renderGame World { worldPlayer = p,
                   worldComputer = c,
                   worldBall = b } =
  renderPaddle p <> renderPaddle c <> renderBall b

-- Render the init splash screen
renderInit :: Picture
renderInit = (translate (-120) 50 $ scale 0.2 0.2 $ color Config.pongGreen $ text "Welcome to Pong!") <>
  renderStart

-- Render the game over screen, based on the last direction of the ball
renderEnd :: Direction -> Picture
renderEnd dir = (translate (-170) 50 $ scale 0.2 0.2 $ color Config.pongGreen $ text $ "Game over! " ++ (if dir == ToPlayer then "Computer" else "Player") ++ " wins.") <>
  renderStart

-- Render the "press to start" message
renderStart :: Picture
renderStart = translate (-80) (-50) $ scale 0.1 0.1 $ color Config.pongGreen $ text "Press SPACE to start"

-- Handle input events
handle :: Event -> World -> World
handle _ w@(World { worldState = StateGame }) = w
handle e w = case e of
  EventKey (SpecialKey KeySpace) Down _ _ -> initWorld { worldState = StateGame }
  _ -> w

-- Step the game forward in the given amount of time
step :: Float -> World -> World
step t w@(World { worldState = StateGame,
                  worldBall = Ball { ballLocation = (x,y) },
                  worldSpeed = v,
                  worldAngle = a }) =
  let dx = t * v * (cos a)
      dy = t * v * (sin a)
  in if isDead x
     then w { worldState = StateEnd }
     else w { worldBall = makeBall (x + dx) (y + dy) }
  where
    isDead :: Float -> Bool
    isDead bx = bx < (fromIntegral Config.width / (-2)) + (ballWidth / 2) || bx > (fromIntegral Config.width / 2) - (ballWidth / 2)
step _ w = w

-- Start the game
startPong :: IO ()
startPong = play (InWindow "Pong!" (Config.width, Config.height) (Config.x, Config.y))
  Config.background Config.frequency
  initWorld render handle step
