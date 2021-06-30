
module Pong.Universe(
    Pong.Universe.universe
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

data Universe = Universe {
    arena :: Arena
}

universe :: Universe.Universe Pong.Universe.Universe
universe = Universe.Universe {
    name = "pong",
    fps = 60,
    resolution = (425, 240),
    adaptedResolution = (425, 240),
    scaleFactor = (1.0, 1.0),
    Universe.spriteSheets = Arena.spriteSheets ++ [Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"],
    Universe.think = Pong.Universe.think,
    playSounds = [],
    drawSprites = [],
    Universe.universe = Pong.Universe.Universe {Pong.Universe.arena = Arena.arena}
}

think :: Universe.Universe Pong.Universe.Universe -> Tactile -> Universe.Universe Pong.Universe.Universe
think u ts = u { playSounds = soundsToPlay ts,
                 drawSprites = spritesToDraw p',
                 Universe.universe = p'
               }
                where p = Universe.universe u
                      p'= Pong.Universe.Universe {Pong.Universe.arena = Arena.think (Pong.Universe.arena p) ts}

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: Pong.Universe.Universe -> [Sprite]
spritesToDraw  u = [Pitch.sprite, Paddle.sprite (lPaddle a), Paddle.sprite (rPaddle a)]
                 where a = Pong.Universe.arena u
