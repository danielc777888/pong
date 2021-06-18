
module Core.Visual where

import Core.Math

type Resolution = (Nat, Nat)
type ScaleFactor = Float
type Position = (Int, Int)
type SpriteSheet = String
type FontFile = String


selectResolution :: Resolution -> [Resolution] -> Resolution
selectResolution x = foldr (\r acc -> if r > acc && r <= x then r else acc) (0,0)

scaleFactor :: Resolution -> Resolution -> (ScaleFactor, ScaleFactor)
scaleFactor (w, h) (w', h') = (ws, hs)
                    where ws = (fromIntegral w') / (fromIntegral w)
                          hs = (fromIntegral h') / (fromIntegral h)

translatePosition :: Position -> (ScaleFactor, ScaleFactor) -> Position
translatePosition (x, y) (ws, hs) = (round (fromIntegral x * ws), round (fromIntegral y * hs))