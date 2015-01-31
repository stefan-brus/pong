-- Main game module

module Pong where

import Control.Monad.Random

import Data.Fixed (mod')
import Data.Monoid

import Graphics.Gloss.Interface.IO.Game

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

-- The state of the player paddle
data PlayerMovement = PlayerStill | PlayerUp | PlayerDown deriving (Eq,Show)

-- Collision direction enumerator
data Collision = NoCollision | HCollision | VCollision deriving (Eq)

-- The world
-- Speed is in pixels per second
data World = World {
  worldState :: State,
  worldPlayer :: Paddle,
  worldComputer :: Paddle,
  worldBall :: Ball,
  worldDir :: Direction,
  worldMoving :: PlayerMovement,
  worldSpeed :: Float,
  worldAngle :: Float,
  worldScore :: Int
}

--------------------------
-- GAME LOGIC FUNCTIONS --
--------------------------

-- Set up the initial state of the world with the given ball angle
initWorld :: World
initWorld = World {
  worldState = StateInit,
  worldPlayer = playerPaddle,
  worldComputer = computerPaddle,
  worldBall = makeBall 0 0,
  worldDir = ToPlayer,
  worldMoving = PlayerStill,
  worldSpeed = 100,
  worldAngle = pi,
  worldScore = 0
}

-- Render the game given the state of the world
render :: World -> IO Picture
render World { worldState = StateInit } = return renderInit
render World { worldState = StateEnd, worldBall = ball } = return $ renderEnd ball
render w@(World { worldState = StateGame, worldScore = s }) = return $ renderGame w <> renderScore s

-- Render the score
renderScore :: Int -> Picture
renderScore s = translate (fromIntegral (-Config.width) / 2 + 70) (fromIntegral (-Config.height) / 2 + 20) $ scale 0.1 0.1 $ color Config.pongGreen $ text $ show s

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

-- Render the game over screen, based on the last location of the ball
renderEnd :: Ball -> Picture
renderEnd Ball { ballLocation = (x,_) } = (translate (-170) 50 $ scale 0.2 0.2 $ color Config.pongGreen $ text $ "Game over! " ++ (if x < 0 then "Computer" else "Player") ++ " wins.") <>
  renderStart

-- Render the "press to start" message
renderStart :: Picture
renderStart = translate (-80) (-50) $ scale 0.1 0.1 $ color Config.pongGreen $ text "Press SPACE to start"

-- Handle input events
handle :: Event -> World -> IO World
handle e w@(World { worldState = StateGame,
                    worldMoving = m }) = case e of
  EventKey (SpecialKey KeyUp) Down _ _ -> return $ w { worldMoving = PlayerUp }
  EventKey (SpecialKey KeyUp) Up _ _ -> return $ w { worldMoving = if m == PlayerUp then PlayerStill else m }
  EventKey (SpecialKey KeyDown) Down _ _ -> return $ w { worldMoving = PlayerDown }
  EventKey (SpecialKey KeyDown) Up _ _ -> return $ w { worldMoving = if m == PlayerDown then PlayerStill else m }
  _ -> return w
handle e w = case e of
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    initAngle <- evalRandIO $ getRandomR (pi / 2,3 * pi / 2)
    return $ initWorld { worldState = StateGame, worldAngle = initAngle }
  _ -> return w

-- Step the game forward in the given amount of time
step :: Float -> World -> IO World
step t w@(World { worldState = StateGame,
                  worldPlayer = pp,
                  worldComputer = pc@(Paddle { paddleHeight = compHeight }),
                  worldBall = Ball { ballLocation = (x,y) },
                  worldDir = d,
                  worldMoving = m,
                  worldSpeed = v,
                  worldAngle = a,
                  worldScore = s }) =
  let dx = t * v * (cos a)
      dy = t * v * (sin a)
      playerDist = if m == PlayerStill then 0 else if m == PlayerUp then t * Config.paddleSpeed else (-t) * Config.paddleSpeed
      compDist = if compHeight <= y + 1 && compHeight >= y - 1 then 0 else if compHeight < y then t * Config.paddleSpeed else (-t) * Config.paddleSpeed
      newBall = makeBall (x + dx) (y + dy)
      (collision,coeff) = checkCollision newBall
      isPlayerBounce = d == ToPlayer && collision == HCollision
  in if isDead x
     then return $ w { worldState = StateEnd }
     else return $ w {
       worldPlayer = movePaddle pp playerDist,
       worldComputer = movePaddle pc compDist,
       worldBall = newBall,
       worldDir = if collision == HCollision then changeDir d else d,
       worldSpeed = v * if isPlayerBounce then 1.1 else 1,
       worldAngle = changeAngle collision coeff,
       worldScore = s + if isPlayerBounce then 1 else 0
     }
  where
    isDead :: Float -> Bool
    isDead bx = bx < (fromIntegral Config.width / (-2)) + paddleWidth || bx > (fromIntegral Config.width / 2) - paddleWidth

    changeDir :: Direction -> Direction
    changeDir ToPlayer = ToComputer
    changeDir ToComputer = ToPlayer

    changeAngle :: Collision -> Float -> Float
    changeAngle VCollision _ = 2 * pi - a
    changeAngle HCollision coeff = ((-pi) - a + pi / 4 * if coeff > 0.5 then 0.5 else coeff) `mod'` (2 * pi)
    changeAngle _ _ = a

    checkCollision :: Ball -> (Collision, Float)
    checkCollision Ball { ballLocation = (bx,by) }
      | bx >= fromIntegral Config.width / 2 - paddleWidth - ballSize / 2 = checkPaddle pc by
      | bx <= fromIntegral (-Config.width) / 2 + paddleWidth + ballSize / 2 = checkPaddle pp by
      | by >= fromIntegral Config.height / 2 - ballSize + 2 || by <= fromIntegral (-Config.height) / 2 + ballSize - 2 = (VCollision,0)
      | otherwise = (NoCollision,0)

    checkPaddle :: Paddle -> Float -> (Collision, Float)
    checkPaddle (Paddle { paddleHeight = h }) by
      | by + ballSize > h - paddleSize && by - ballSize < h + paddleSize = (HCollision,1 - (paddleSize - (by - h)) / paddleSize)
      | otherwise = (NoCollision,0)
step _ w = return w

-- Start the game
startPong :: IO ()
startPong = playIO (InWindow "Pong!" (Config.width, Config.height) (Config.x, Config.y))
  Config.background Config.frequency
  initWorld render handle step
