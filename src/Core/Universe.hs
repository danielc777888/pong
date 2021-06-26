module Core.Universe(
    Universe(..)
) where

import Core.Existent
import Core.Auditory
import Core.Visual
import Core.Tactile
import Core.Math

data Universe u = Universe {
    --common data
     uName :: Name,
     uFps :: Nat,
     uResolution :: Resolution,
     uAdaptedResolution :: Resolution,
     uScaleFactor :: (ScaleFactor, ScaleFactor),
     uSpriteSheets :: [SpriteSheet],
     uFonts :: [FontFile],
     uSounds :: [SoundFile],
     uMusic :: [MusicFile],
     uThink :: Universe u -> Tactile -> Universe u,
     uPlaySounds :: [SoundFile],
     uDrawSprites :: [Sprite],
     --polymorphic data
     uUniverse :: u
}
