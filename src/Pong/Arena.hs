module Pong.Arena(
    Arena(..),
    spriteSheets,
    arena,
    think
) where

import Core.Math
import Core.Visual
import Core.Tactile

import Pong.Ball
import Pong.Paddle
import Pong.Pitch

data Arena = Arena {
    lPaddle :: Paddle,
    rPaddle :: Paddle
}

arena :: Arena
arena = Arena {
        lPaddle = paddle { paddleName = "lPaddle", paddleSourcePosition = Vector {x = 3, y = 2}, paddleTargetPosition = Vector {x = 5, y = 100}},
        rPaddle = paddle { paddleName = "rPaddle", paddleSourcePosition = Vector {x = 3, y = 2}, paddleTargetPosition = Vector {x = 410, y = 100}}
}

spriteSheets :: [SpriteSheet]
spriteSheets = [Pong.Ball.spriteSheet] ++ [Pong.Paddle.spriteSheet] ++ [Pong.Pitch.spriteSheet]

think :: Arena -> Tactile -> Arena
think a ts = a {
    lPaddle = move (lPaddle a) ts (Key_A, Key_Z),
    rPaddle = move (rPaddle a) ts (Key_Up, Key_Down)
}



