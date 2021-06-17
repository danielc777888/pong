
module Pong.Universe where

import Core.Universe

import Pong.Arena
import Pong.Start

universe :: Universe
universe = Universe {
    name = "pong",
    resolution = (425, 240),
    Core.Universe.spriteSheets = Pong.Arena.spriteSheets ++ [Pong.Start.spriteSheet],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"]    
}