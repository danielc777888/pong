module Pong.Arena(
    Arena(..),
    spriteSheets,
    arena,
    think
) where

import Core.Math
import Core.Visual (SpriteSheet)
import Core.Tactile

import Pong.Ball as Ball
import Pong.Paddle as Paddle
import Pong.Pitch as Pitch

data Arena = Arena {
    lPaddle :: Paddle,
    rPaddle :: Paddle
}

arena :: Arena
arena = Arena {
        lPaddle = paddle { name = "lPaddle", sourcePosition = Vector 3 2, targetPosition = Vector 5 100},
        rPaddle = paddle { name = "rPaddle", sourcePosition = Vector 3 32, targetPosition = Vector 410 100}
}

spriteSheets :: [SpriteSheet]
spriteSheets = [Ball.spriteSheet] ++ [Paddle.spriteSheet paddle] ++ [Pitch.spriteSheet]

think :: Arena -> Tactile -> Arena
think a ts = a {
    lPaddle = move (lPaddle a) ts (Key_A, Key_Z),
    rPaddle = move (rPaddle a) ts (Key_Up, Key_Down)
}



