
module Pong.PongUniverse(
    pongUniverse
) where

import Data.Set

import Core.Universe as Universe
import Core.Tactile
import Core.Visual (Sprite)
import Core.Auditory
import Core.Math

import Pong.Arena as Arena
import Pong.Start as Start
import Pong.Pitch as Pitch
import Pong.Paddle as Paddle (sprite)

data PongUniverse = PongUniverse {
    arena :: Arena
}

pongUniverse :: Universe PongUniverse
pongUniverse = Universe {
    name = "pong",
    fps = 60,
    resolution = (425, 240),
    adaptedResolution = (425, 240),
    scaleFactor = (1.0, 1.0),
    Universe.spriteSheets = Arena.spriteSheets ++ [Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"],
    Universe.think = Pong.PongUniverse.think,
    playSounds = [],
    drawSprites = [],
    universe = PongUniverse {Pong.PongUniverse.arena = Arena.arena}
}

think :: Universe PongUniverse -> Tactile -> Universe PongUniverse
think u ts = u { playSounds = soundsToPlay ts,
                 drawSprites = spritesToDraw p',
                 universe = p'
               }
                where p = universe u
                      p'= PongUniverse {Pong.PongUniverse.arena = Arena.think (Pong.PongUniverse.arena p) ts}

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: PongUniverse -> [Sprite]
spritesToDraw  u = [Pitch.sprite, Paddle.sprite (lPaddle a), Paddle.sprite (rPaddle a)]
                 where a = Pong.PongUniverse.arena u
