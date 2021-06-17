module Core.Universe where

import Core.Visual

type SpriteSheet = String
type SoundFile = String
type FontFile = String
type MusicFile = String

data Universe = Universe {
     name :: String,
     resolution :: Resolution,
     spriteSheets :: [SpriteSheet],
     fonts :: [FontFile],
     sounds :: [SoundFile],
     music :: [MusicFile]
    }
