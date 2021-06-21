module Pong.Paddle(
    Pong.Paddle.spriteSheet,
    Pong.Paddle.Paddle (..),
    toSprite,
    paddle,
    move
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
spriteSheet = "paddle"

paddle :: Paddle 
paddle = Paddle {
    paddleName = "paddle",
    paddleSpriteSheet = Pong.Paddle.spriteSheet,
    paddleSourcePosition = Vector {x = 0, y = 0},
    paddleTargetPosition = Vector {x = 410, y = 100},
    paddleDimensions =  (10, 26)
}

toSprite :: Paddle -> Sprite
toSprite p = Sprite {
    Core.Visual.spriteSheet = paddleSpriteSheet p,
    sourcePosition = paddleSourcePosition p,
    targetPosition = paddleTargetPosition p,
    dimensions =  paddleDimensions p
}

move :: Paddle -> Tactile -> Paddle
move p ts
    -- | touched Key_Up ts
    | S.member Key_Up  (keysPressed ts) || S.member Key_Up  (keysDown ts) = p { paddleTargetPosition = moveUp (paddleTargetPosition p)}
    | S.member Key_Down (keysPressed ts) || S.member Key_Down  (keysDown ts) = p { paddleTargetPosition = moveDown (paddleTargetPosition p)}
    | otherwise = p

moveUp :: Position -> Position
moveUp p = p { y = (y p) - 3 }

moveDown :: Position -> Position
moveDown p = p { y = (y p) + 3 }