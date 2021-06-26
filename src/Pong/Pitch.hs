
module Pong.Pitch(
    pitchSpriteSheet,
    pitchSprite
) where

import Core.Visual
import Core.Math

pitchSpriteSheet :: SpriteSheet
pitchSpriteSheet = "pitch"

pitchSprite :: Sprite
pitchSprite = Sprite { spriteSheet = pitchSpriteSheet,
 sourcePosition = Vector {x = 0, y = 0},
 targetPosition = Vector {x = 0, y = 0},
 dimensions =  (425, 240) }