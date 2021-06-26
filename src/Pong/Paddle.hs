module Pong.Paddle(
    Pong.Paddle.spriteSheet,
    Pong.Paddle.Paddle (..),
    paddle,
    move,
    sprite
) where

import qualified Data.Set as S

import Core.Math
import Core.Visual
import Core.Existent
import Core.Universe
import Core.Tactile

data Paddle = Paddle {
    paddleName :: Name,
    paddleSpriteSheet :: SpriteSheet,
    paddleSourcePosition :: Vector,
    paddleTargetPosition :: Vector,
    paddleDimensions :: Dimensions
}

spriteSheet :: SpriteSheet
spriteSheet = "paddles"

paddle :: Paddle
paddle = Paddle {
    paddleName = "paddle",
    paddleSpriteSheet = Pong.Paddle.spriteSheet,
    paddleSourcePosition = zeroVector,
    paddleTargetPosition = zeroVector,
    paddleDimensions =  (10, 26)
}

sprite :: Paddle -> Sprite
sprite p = Sprite {
    Core.Visual.spriteSheet = paddleSpriteSheet p,
    sourcePosition = paddleSourcePosition p,
    targetPosition = paddleTargetPosition p,
    dimensions =  paddleDimensions p
}

move :: Paddle -> Tactile -> (KeyboardKey, KeyboardKey) -> Paddle
move p ts (u, d)
    | touchedKey u ts = p { paddleTargetPosition = moveUp (paddleTargetPosition p)}
    | touchedKey d ts = p { paddleTargetPosition = moveDown (paddleTargetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp p = p { y = (y p) - 3 }

moveDown :: Position -> Position
moveDown p = p { y = (y p) + 3 }