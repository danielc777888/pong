
module Core.Visual(
    Resolution,
    ScaleFactor,
    Position,
    Dimensions,
    SpriteSheet,
    FontFile,
    Sprite(..),
    selectResolution,
    scaleFactor
) where

import Core.Math
import Core.Existent

type Resolution = (Nat, Nat)
type ScaleFactor = Float
type Position = Vector
type Dimensions = (Nat, Nat)
type SpriteSheet = Name
type FontFile = Name

data Sprite = Sprite {
    spriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    targetPosition :: Position,
    dimensions :: Dimensions
}

selectResolution :: Resolution -> [Resolution] -> Resolution
selectResolution x = foldr (\r acc -> if r > acc && r <= x then r else acc) (0,0)

scaleFactor :: Resolution -> Resolution -> (ScaleFactor, ScaleFactor)
scaleFactor (w, h) (w', h') = (ws, hs)
                    where ws = (fromIntegral w') / (fromIntegral w)
                          hs = (fromIntegral h') / (fromIntegral h)
