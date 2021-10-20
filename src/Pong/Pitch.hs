module Pong.Pitch
  ( pthSpriteSheet,
    pthSprite,
  )
where

import Core.Math (Vector (Vector))
import Core.Visual (Sprite (Sprite), SpriteSheet)

pthSpriteSheet :: SpriteSheet
pthSpriteSheet = "pitch"

pthSprite :: Sprite
pthSprite = Sprite pthSpriteSheet (Vector 0 0) (Vector 0 0) (425, 240)