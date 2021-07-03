module Core.Universe(
    Universe(..)
) where

import Core.Existent
import Core.Auditory
import Core.Visual
import Core.Tactile
import Core.Math
import Core.Time (Time(..))

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
     think :: Universe ws -> Time -> Tactile -> Universe ws,
     playSounds :: [SoundFile],
     drawSprites :: [Sprite],
     timeFactor :: Float,
     --polymorphic data
     worlds :: ws
}
