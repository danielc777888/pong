module Core.Universe where

import Core.Existent
import Core.Auditory
import Core.Visual
import Core.Tactile
import Core.Math

data Universe a = Universe {
     name :: Name,
     fps :: Nat,
     resolution :: Resolution,
     adaptedResolution :: Resolution,
     scaleFactor :: (ScaleFactor, ScaleFactor),
     spriteSheets :: [SpriteSheet],
     fonts :: [FontFile],
     sounds :: [SoundFile],
     music :: [MusicFile],
     think :: Universe a -> [Tactile] -> Universe a,
     playSounds :: [SoundFile],
     drawSprites :: [Sprite],
     gameState :: a
}
