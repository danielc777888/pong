
module Pong.Pitch(
    Pong.Pitch.spriteSheet,
    sprite
) where

import Core.Visual as Visual
import Core.Math

spriteSheet :: SpriteSheet
spriteSheet = "pitch"

sprite :: Sprite
sprite = Sprite {
    Visual.spriteSheet = Pong.Pitch.spriteSheet,
    sourcePosition = Vector 0 0,
    targetPosition = Vector 0 0,
    dimensions =  (425, 240)
}