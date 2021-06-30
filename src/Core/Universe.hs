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
     name :: Name,
     fps :: Nat,
     resolution :: Resolution,
     adaptedResolution :: Resolution,
     scaleFactor :: (ScaleFactor, ScaleFactor),
     spriteSheets :: [SpriteSheet],
     fonts :: [FontFile],
     sounds :: [SoundFile],
     music :: [MusicFile],
     think :: Universe u -> Tactile -> Universe u,
     playSounds :: [SoundFile],
     drawSprites :: [Sprite],
     --polymorphic data
     universe :: u
}
