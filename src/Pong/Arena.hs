module Pong.Arena(
    Arena(..),
    spriteSheets,
    arena,
    Pong.Arena.think
) where

import Core.Math
import Core.Visual (SpriteSheet)
import Core.Tactile
import Core.Time (Time(..))

import Pong.Ball as Ball
import Pong.Paddle as Paddle (Paddle(..), spriteSheet, lPaddle, rPaddle, paddle, think)
import Pong.Pitch as Pitch

data Arena = Arena {
    lPaddle :: Paddle,
    rPaddle :: Paddle
}

arena :: Arena
arena = Arena {
        Pong.Arena.lPaddle = Paddle.lPaddle { targetPosition = Vector 5 100 },
        Pong.Arena.rPaddle = Paddle.rPaddle { targetPosition = Vector 410 100 }
}

spriteSheets :: [SpriteSheet]
spriteSheets = [Ball.spriteSheet] ++ [Paddle.spriteSheet paddle] ++ [Pitch.spriteSheet]

think :: Arena -> Time -> Tactile -> Arena
think (Arena lp rp) t ts = Arena (Paddle.think lp t ts) (Paddle.think rp t ts)



