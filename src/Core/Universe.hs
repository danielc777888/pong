module Core.Universe
  ( Universe (..),
  )
where

import Core.Auditory (MusicFile, SoundFile)
import Core.Existent (Name)
import Core.Math (Nat)
import Core.Physics (CollisionBox)
import Core.Tactile (Tactile)
import Core.Time (Time)
import Core.Visual (FontFile, Resolution, ScaleFactor, Sprite, SpriteSheet)

data Universe ws = Universe
  { --common data
    name :: Name,
    fps :: Nat,
    resolution :: Resolution,
    adaptedResolution :: Resolution,
    uniScaleFactor :: (ScaleFactor, ScaleFactor),
    uniSpriteSheets :: [SpriteSheet],
    fonts :: [FontFile],
    sounds :: [SoundFile],
    music :: [MusicFile],
    uniThink :: Universe ws -> Time -> Tactile -> Universe ws,
    playSounds :: [SoundFile],
    drawSprites :: [Sprite],
    timeFactor :: Float,
    collisionBoxes :: [CollisionBox],
    randomValues :: [((Nat, Nat), Nat)],
    --polymorphic data
    worlds :: ws
  }
