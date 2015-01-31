-- Pong configuration

module Pong.Config where

import Graphics.Gloss

-- Window width
width :: Int
width = 400

-- Window height
height :: Int
height = 300

-- Window starting position x
x :: Int
x = 200

-- Window starting position y
y :: Int
y = 200

-- Window background color
background :: Color
background = black

-- Game loop ticks per second
frequency :: Int
frequency = 60

-- The "pong green" color
pongGreen :: Color
pongGreen = makeColorI 0x2B 0xA8 0x20 0xFF

-- How fast the paddles can go in pixels/sec
paddleSpeed :: Float
paddleSpeed = 150
