
module Pong.Universe(
    universe
) where

import Core.Universe
import Core.Tactile
import Core.Visual
import Core.Auditory

import Pong.Arena
import Pong.Start

universe :: Universe
universe = Universe {
    name = "pong",
    resolution = (425, 240),
    adaptedResolution = (425, 240),
    Core.Universe.scaleFactor = (1.0, 1.0),
    Core.Universe.spriteSheets = Pong.Arena.spriteSheets ++ [Pong.Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"],
    Core.Universe.think = Pong.Universe.think,
    playSounds = [],
    drawSprites = []
}

think :: Universe -> [Tactile] -> Universe
think u ts = u { playSounds = soundsToPlay ts, drawSprites = spritesToDraw }

soundsToPlay :: [Tactile] -> [SoundFile]
soundsToPlay [] = []
soundsToPlay ts = if Space `elem` ts then ["sound"] else []

spritesToDraw :: [Sprite]
spritesToDraw = [Sprite { Core.Visual.spriteSheet = "pitch",
 sourcePosition = (0, 0),
 targetPosition = (0, 0),
 dimensions =  (425, 240) }, 
 Sprite {
     Core.Visual.spriteSheet = "paddle",
    sourcePosition = (0, 0),
    targetPosition = (5, 100),
    dimensions =  (10, 26)
 }]