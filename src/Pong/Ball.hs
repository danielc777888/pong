module Pong.Ball(
    Ball(..),
    ball,
    think,
    sprite
) where

import Core.Visual (SpriteSheet)
import Core.Existent (Name)
import Core.Math (Nat, Vector(..), zeroVector)
import Core.Visual (Position, Dimensions, Sprite(Sprite))

data Ball = Ball {
    name :: Name,
    spriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    targetPosition :: Position,
    dimensions :: Dimensions,
    currentFrame :: Nat,
    currentFrameStartTime :: Double,
    speed :: Nat
}

ball :: Ball
ball = Ball "ball" "ball" zeroVector zeroVector (6, 6) 0 0 200

think :: Ball -> Nat -> Ball
think b rv = b {targetPosition = Vector (x tp) rv }
             where tp = targetPosition b

sprite :: Ball -> Sprite
sprite b = Sprite (spriteSheet b) (sourcePosition b) (targetPosition b) (dimensions b)