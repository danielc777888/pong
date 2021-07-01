module Pong.Paddle(
    Paddle (..),
    paddle,
    move,
    sprite
) where

import Data.Set

import Core.Math (Vector(Vector), zeroVector)
import Core.Visual (SpriteSheet, Sprite(Sprite), Position, Dimensions)
import Core.Existent (Name)
import Core.Tactile (Tactile, KeyboardKey, touchedKey)

data Paddle = Paddle {
    name :: Name,
    spriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    targetPosition :: Position,
    dimensions :: Dimensions
}

paddle :: Paddle
paddle = Paddle "paddle" "paddles" zeroVector zeroVector (10, 26)

sprite :: Paddle -> Sprite
sprite (Paddle _ ss sp tp d) = Sprite ss sp tp d

move :: Paddle -> Tactile -> (KeyboardKey, KeyboardKey) -> Paddle
move p ts (u, d)
    | touchedKey u ts = p { targetPosition = moveUp (targetPosition p)}
    | touchedKey d ts = p { targetPosition = moveDown (targetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp (Vector x y) = Vector x (y - 3)

moveDown :: Position -> Position
moveDown (Vector x y) = Vector x (y + 3)