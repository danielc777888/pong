module Pong.Pitch
  ( spriteSheet,
    sprite,
  )
where

import Core.Math (Vector (Vector))
import Core.Visual as Visual (Sprite (Sprite), SpriteSheet)

spriteSheet :: SpriteSheet
spriteSheet = "pitch"

sprite :: Sprite
sprite = Sprite spriteSheet (Vector 0 0) (Vector 0 0) (425, 240)