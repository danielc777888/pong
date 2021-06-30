module Pong.Paddle(
    Paddle (..),
    paddle,
    move,
    sprite
) where

import Data.Set

import Core.Math (Vector, x, y, zeroVector)
import Core.Visual as Visual
import Core.Existent
import Core.Tactile

data Paddle = Paddle {
    name :: Name,
    spriteSheet :: SpriteSheet,
    sourcePosition :: Vector,
    targetPosition :: Vector,
    dimensions :: Dimensions
}

paddle :: Paddle
paddle = Paddle {
    name = "paddle",
    Pong.Paddle.spriteSheet = "paddles",
    Pong.Paddle.sourcePosition = zeroVector,
    Pong.Paddle.targetPosition = zeroVector,
    Pong.Paddle.dimensions =  (10, 26)
}

sprite :: Paddle -> Sprite
sprite p = Sprite {
    Visual.spriteSheet = Pong.Paddle.spriteSheet p,
    Visual.sourcePosition = Pong.Paddle.sourcePosition p,
    Visual.targetPosition = Pong.Paddle.targetPosition p,
    Visual.dimensions =  Pong.Paddle.dimensions p
}

move :: Paddle -> Tactile -> (KeyboardKey, KeyboardKey) -> Paddle
move p ts (u, d)
    | touchedKey u ts = p { Pong.Paddle.targetPosition = moveUp (Pong.Paddle.targetPosition p)}
    | touchedKey d ts = p { Pong.Paddle.targetPosition = moveDown (Pong.Paddle.targetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp p = p {y = y'}
           where y' = (y p) - 3

moveDown :: Position -> Position
moveDown p = p {y = y'}
             where y' = (y p) + 3
