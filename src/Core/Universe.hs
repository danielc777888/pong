module Core.Universe where

import Core.Existent
import Core.Auditory
import Core.Visual
import Core.Tactile

data Universe = Universe {
     name :: Name,
     resolution :: Resolution,
     adaptedResolution :: Resolution,
     scaleFactor :: (ScaleFactor, ScaleFactor),
     spriteSheets :: [SpriteSheet],
     fonts :: [FontFile],
     sounds :: [SoundFile],
     music :: [MusicFile],
     think :: Universe -> [Tactile] -> Universe
    }
