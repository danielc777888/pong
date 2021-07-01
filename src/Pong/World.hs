
module Pong.World(
    universe
) where

import Data.Set

import Core.Universe as Universe (Universe(..))
import Core.Tactile
import Core.Visual (Sprite)
import Core.Auditory
import Core.Math

import Pong.Arena as Arena
import Pong.Start as Start
import Pong.Pitch as Pitch
import Pong.Paddle as Paddle (sprite)

newtype World = World Arena

universe :: Universe World
universe = Universe {
    name = "pong",
    fps = 60,
    resolution = (425, 240),
    adaptedResolution = (425, 240),
    scaleFactor = (1.0, 1.0),
    Universe.spriteSheets = Arena.spriteSheets ++ [Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"],
    Universe.think = Pong.World.think,
    playSounds = [],
    drawSprites = [],
    worlds = World arena
}

think :: Universe World -> Tactile -> Universe World
think u ts = u { playSounds = soundsToPlay ts,
                 drawSprites = spritesToDraw p',
                 worlds = p'
               }
                where (World a) = worlds u
                      p'= World (Arena.think a ts)

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: World -> [Sprite]
spritesToDraw (World a) = [Pitch.sprite, Paddle.sprite (lPaddle a), Paddle.sprite (rPaddle a)]
