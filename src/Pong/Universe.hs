
module Pong.Universe where

import Core.Universe

universe :: Universe
universe = Universe {
    name = "pong",
    resolution = (425, 240),
    spriteSheets = ["ball","paddle", "pitch", "start"],
    fonts = ["alagard"],
    sounds = ["sound"],
    music =  ["target"]    
}