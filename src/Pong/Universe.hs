
module Pong.Universe(
    universe
) where

import qualified Data.Set as S

import Core.Universe
import Core.Tactile
import Core.Visual
import Core.Auditory
import Core.Math

import Pong.Arena
import Pong.Start
import Pong.Pitch
import Pong.Paddle

data PongUniverse = PongUniverse {
    arena :: Arena
}

universe :: Universe PongUniverse
universe = Universe {
    name = "pong",
    fps = 60,
    resolution = (425, 240),
    adaptedResolution = (425, 240),
    Core.Universe.scaleFactor = (1.0, 1.0),
    Core.Universe.spriteSheets = Pong.Arena.spriteSheets ++ [Pong.Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"],
    Core.Universe.think = Pong.Universe.think,
    playSounds = [],
    drawSprites = [],
    cosmos = PongUniverse {Pong.Universe.arena = Pong.Arena.arena}
}

think :: Universe PongUniverse -> Tactile -> Universe PongUniverse
think u ts = u { playSounds = soundsToPlay ts,
                 drawSprites = spritesToDraw c',
                 cosmos = c'
               }
                where c = cosmos u
                      c'= PongUniverse {Pong.Universe.arena = Pong.Arena.think (Pong.Universe.arena c) ts}

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if S.member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: PongUniverse -> [Sprite]
spritesToDraw  u = [Pong.Pitch.sprite, Pong.Paddle.sprite (lPaddle a), Pong.Paddle.sprite (rPaddle a)]
                 where a = Pong.Universe.arena u
