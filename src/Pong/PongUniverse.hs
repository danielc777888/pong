
module Pong.PongUniverse(
    pongUniverse
) where

import Data.Set

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
    uArena :: Arena
}

pongUniverse :: Universe PongUniverse
pongUniverse = Universe {
    uName = "pong",
    uFps = 60,
    uResolution = (425, 240),
    uAdaptedResolution = (425, 240),
    uScaleFactor = (1.0, 1.0),
    uSpriteSheets = arenaSpriteSheets ++ [startSpriteSheet],
    uFonts = ["alagard"],
    uSounds = ["sound"],
    uMusic =  ["target"],
    uThink = think,
    uPlaySounds = [],
    uDrawSprites = [],
    uUniverse = PongUniverse {uArena = arena}
}

think :: Universe PongUniverse -> Tactile -> Universe PongUniverse
think u ts = u { uPlaySounds = soundsToPlay ts,
                 uDrawSprites = spritesToDraw p',
                 uUniverse = p'
               }
                where p = uUniverse u
                      p'= PongUniverse {uArena = arenaThink (uArena p) ts}

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: PongUniverse -> [Sprite]
spritesToDraw  u = [pitchSprite, paddleSprite (lPaddle a), paddleSprite (rPaddle a)]
                 where a = uArena u
