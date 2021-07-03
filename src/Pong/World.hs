
module Pong.World(
    universe
) where

import Data.Set

import Core.Universe as Universe (Universe(..))
import Core.Tactile
import Core.Visual (Sprite)
import Core.Time (Time(..))
import Core.Auditory
import Core.Math

import Pong.Arena as Arena
import Pong.Start as Start
import Pong.Pitch as Pitch
import Pong.Paddle as Paddle (sprite)

newtype World = World {arena :: Arena}

universe :: Universe World
universe = Universe {
    name = "pong",
    fps = 30,
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
    timeFactor = 1.0,
    worlds = World Arena.arena
}

think :: Universe World -> Time -> Tactile -> Universe World
think u t ts = u { playSounds = soundsToPlay ts,
                    drawSprites = spritesToDraw w,
                    worlds = w }
                where a = Pong.World.arena (worlds u)
                      w = World (Arena.think a t ts)

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: World -> [Sprite]
spritesToDraw (World a) = [Pitch.sprite, Paddle.sprite (lPaddle a), Paddle.sprite (rPaddle a)]
