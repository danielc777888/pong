module Pong.Paddle(
    Paddle (..),
    paddle, lPaddle, rPaddle,
    think,
    sprite
) where

import Data.Set

import Core.Math (Nat, Vector(Vector), zeroVector, y)
import Core.Visual (
    SpriteSheet,
    Sprite(Sprite),
    Position,
    Dimensions,
    Animation,
    Frame(..)
    )
import Core.Existent (Name)
import Core.Tactile (Tactile, KeyboardKey(..), touchedKey)
import Core.Time (Age)

data Paddle = Paddle {
    name :: Name,
    spriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    targetPosition :: Position,
    dimensions :: Dimensions,
    animation :: Animation,
    upKey :: KeyboardKey,
    downKey :: KeyboardKey,
    currentFrame :: Nat,
    currentFrameStartTime :: Age
}

paddle :: Paddle
paddle = Paddle "paddle" "paddles" zeroVector zeroVector (10, 26) [] Key_Null Key_Null 0 0

lPaddle :: Paddle
lPaddle = paddle { name = "lPaddle", sourcePosition = Vector 3 2, animation = an, upKey = Key_A, downKey = Key_Z }
        where an = [Frame 0 (Vector 3 2) (10, 26) 0.1, Frame 1 (Vector 19 2) (10, 26) 0.2, Frame 2 (Vector 35 2) (10, 26) 0.2, Frame 3 (Vector 51 2) (10, 26) 0.2]

rPaddle :: Paddle
rPaddle = paddle { name = "rPaddle", sourcePosition = Vector 3 32, animation = an, upKey = Key_Up, downKey = Key_Down }
        where an = [Frame 0 (Vector 3 32) (10, 26) 0.1, Frame 1 (Vector 19 32) (10, 26) 0.2, Frame 2 (Vector 35 32) (10, 26) 0.2, Frame 3 (Vector 51 32) (10, 26) 0.2]

think :: Paddle -> Age -> Tactile -> Paddle
think p ag ts = a
            where m = move p ts (upKey p, downKey p)
                  a = animate m ag

sprite :: Paddle -> Sprite
sprite p = Sprite (spriteSheet p)  (fSourcePosition fr) (targetPosition p) (fDimensions fr)
            where fr = (animation p)!!(currentFrame p)

move :: Paddle -> Tactile -> (KeyboardKey, KeyboardKey) -> Paddle
move p ts (u, d)
    | touchedKey u ts = p { targetPosition = moveUp (targetPosition p)}
    | touchedKey d ts = p { targetPosition = moveDown (targetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp (Vector x y) = Vector x (y - 3)

moveDown :: Position -> Position
moveDown (Vector x y) = Vector x (y + 3)

animate :: Paddle -> Double -> Paddle
animate p t = if (frElapsed p t fr) then p { currentFrame = nextFr p, currentFrameStartTime = t } else p
            where fr = (animation p)!!(currentFrame p)
                  nextFr p = (currentFrame p + 1) `mod` (length (animation p))
                  frElapsed p t fr = (t - (currentFrameStartTime p)) > (duration fr)