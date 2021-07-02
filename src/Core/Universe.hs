module Core.Universe(
    Universe(..)
) where

import Core.Existent
import Core.Auditory
import Core.Visual
import Core.Tactile
import Core.Math
import Core.Time

data Universe ws = Universe {
    --common data
     name :: Name,
     fps :: Nat,
     resolution :: Resolution,
     adaptedResolution :: Resolution,
     scaleFactor :: (ScaleFactor, ScaleFactor),
     spriteSheets :: [SpriteSheet],
     fonts :: [FontFile],
     sounds :: [SoundFile],
     music :: [MusicFile],
     think :: Universe ws -> Age -> Tactile -> Universe ws,
     playSounds :: [SoundFile],
     drawSprites :: [Sprite],
     --polymorphic data
     worlds :: ws
}
