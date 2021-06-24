
module Pong.Pitch(
    Pong.Pitch.spriteSheet,
    sprite
) where

import Core.Visual
import Core.Math

spriteSheet :: SpriteSheet
spriteSheet = "pitch"

sprite :: Sprite
sprite = Sprite { Core.Visual.spriteSheet = "pitch",
 sourcePosition = Vector {x = 0, y = 0},
 targetPosition = Vector {x = 0, y = 0},
 dimensions =  (425, 240) }