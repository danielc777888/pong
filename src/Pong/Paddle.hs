module Pong.Paddle(
    paddleSpriteSheet,
    Pong.Paddle.Paddle (..),
    paddle,
    move,
    paddleSprite
) where

import Data.Set

import Core.Math
import Core.Visual
import Core.Existent
import Core.Universe
import Core.Tactile

data Paddle = Paddle {
    pName :: Name,
    pSpriteSheet :: SpriteSheet,
    pSourcePosition :: Vector,
    pTargetPosition :: Vector,
    pDimensions :: Dimensions
}

paddleSpriteSheet :: SpriteSheet
paddleSpriteSheet = "paddles"

paddle :: Paddle
paddle = Paddle {
    pName = "paddle",
    pSpriteSheet = paddleSpriteSheet,
    pSourcePosition = zeroVector,
    pTargetPosition = zeroVector,
    pDimensions =  (10, 26)
}

paddleSprite :: Paddle -> Sprite
paddleSprite p = Sprite {
    spriteSheet = pSpriteSheet p,
    sourcePosition = pSourcePosition p,
    targetPosition = pTargetPosition p,
    dimensions =  pDimensions p
}

move :: Paddle -> Tactile -> (KeyboardKey, KeyboardKey) -> Paddle
move p ts (u, d)
    | touchedKey u ts = p { pTargetPosition = moveUp (pTargetPosition p)}
    | touchedKey d ts = p { pTargetPosition = moveDown (pTargetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp p = p { y = (y p) - 3 }

moveDown :: Position -> Position
moveDown p = p { y = (y p) + 3 }