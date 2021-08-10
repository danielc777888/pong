module Core.Universe
  ( Universe (..),
  )
where

import Core.Auditory
import Core.Existent
import Core.Math
import Core.Physics (CollisionBox (..))
import Core.Tactile
import Core.Time (Time (..))
import Core.Visual

data Universe ws = Universe
  { --common data
    name :: Name,
    fps :: Nat,
    resolution :: Resolution,
    adaptedResolution :: Resolution,
    scaleFactor :: (ScaleFactor, ScaleFactor),
    spriteSheets :: [SpriteSheet],
    fonts :: [FontFile],
    sounds :: [SoundFile],
    music :: [MusicFile],
    think :: Universe ws -> Time -> Tactile -> Universe ws,
    playSounds :: [SoundFile],
    drawSprites :: [Sprite],
    timeFactor :: Float,
    collisionBoxes :: [CollisionBox],
    randomValue :: Nat,
    --polymorphic data
    worlds :: ws
  }
