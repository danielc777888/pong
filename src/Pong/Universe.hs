
module Pong.Universe(
    universe
) where

import Core.Universe
import Core.Tactile
import Core.Visual
import Core.Auditory
import Core.Math

import Pong.Arena
import Pong.Start
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
    gameState = PongUniverse {Pong.Universe.arena = Pong.Arena.arena}
}

think :: Universe PongUniverse -> [Tactile] -> Universe PongUniverse
think u ts = u { playSounds = soundsToPlay ts, drawSprites = spritesToDraw gs, gameState = PongUniverse {Pong.Universe.arena = Pong.Arena.think (Pong.Universe.arena gs) ts}}
                where gs = gameState u
    
    
             --where pu = Pong.Paddle.think u ts

soundsToPlay :: [Tactile] -> [SoundFile]
soundsToPlay [] = []
soundsToPlay ts = if Space `elem` ts then ["sound"] else []

spritesToDraw :: PongUniverse -> [Sprite]
spritesToDraw  u = [pitch, Pong.Paddle.toSprite lPaddle, Pong.Paddle.toSprite rp]
                 where a = Pong.Universe.arena u
                       rp = rPaddle a

pitch :: Sprite
pitch = Sprite { Core.Visual.spriteSheet = "pitch",
 sourcePosition = Vector {x = 0, y = 0},
 targetPosition = Vector {x = 0, y = 0},
 dimensions =  (425, 240) }