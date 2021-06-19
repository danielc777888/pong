module Pong.Arena(
    Arena(..),
    spriteSheets,
    arena,
    think,
    lPaddle
) where

import Core.Math
import Core.Visual
import Core.Tactile

import Pong.Ball
import Pong.Paddle
import Pong.Pitch

data Arena = Arena {
    rPaddle :: Paddle
}

think :: Arena -> [Tactile] -> Arena
think a ts = a { rPaddle = move rp ts}
              where rp = rPaddle a

spriteSheets :: [SpriteSheet]
spriteSheets = [Pong.Ball.spriteSheet] ++ [Pong.Paddle.spriteSheet] ++ [Pong.Pitch.spriteSheet]

arena :: Arena
arena = Arena { rPaddle = paddle { paddleName = "rPaddle", paddleTargetPosition = Vector {x = 410, y = 100}}}

lPaddle :: Paddle
lPaddle = paddle { paddleName = "lPaddle", paddleTargetPosition = Vector {x = 5, y = 100}}
