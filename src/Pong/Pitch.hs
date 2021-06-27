
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
 sourcePosition = Vector 0 0,
 targetPosition = Vector 0 0,
 dimensions =  (425, 240) }