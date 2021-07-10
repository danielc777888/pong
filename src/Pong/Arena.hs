module Pong.Arena(
    Arena(..),
    spriteSheets,
    arena,
    Pong.Arena.think,
    getCollisionBoxes
) where

import Core.Math
import Core.Visual (SpriteSheet)
import Core.Tactile
import Core.Time (Time(..))
import Core.Physics (CollisionBox(..))

import Pong.Ball as Ball
import Pong.Paddle as Paddle (Paddle(..), spriteSheet, lPaddle, rPaddle, paddle, think, collisionBox)
import Pong.Pitch as Pitch
import Pong.Ball as Ball

data Arena = Arena {
    lPaddle :: Paddle,
    rPaddle :: Paddle,
    topWall :: CollisionBox,
    bottomWall :: CollisionBox,
    ball :: Ball
}

arena :: Arena
arena = Arena {
        Pong.Arena.lPaddle = Paddle.lPaddle { Paddle.targetPosition = Vector 5 100 },
        Pong.Arena.rPaddle = Paddle.rPaddle { Paddle.targetPosition = Vector 410 100 },
        topWall = CollisionBox (Vector 0 (-1)) (Vector 425 (-6)),
        bottomWall = CollisionBox (Vector 0 246) (Vector 425 241),
        Pong.Arena.ball = Ball.ball { Ball.targetPosition = Vector 212 120 }
}

spriteSheets :: [SpriteSheet]
spriteSheets = [Ball.spriteSheet Ball.ball] ++ [Paddle.spriteSheet paddle] ++ [Pitch.spriteSheet]

think :: Arena -> Time -> Tactile -> Nat -> Arena
think (Arena lp rp tw bw b) t ts rv = Arena (Paddle.think lp tw bw t ts) (Paddle.think rp tw bw t ts) tw bw (Ball.think b rv)

getCollisionBoxes :: Arena -> [CollisionBox]
getCollisionBoxes (Arena lp rp tw bw b) = [tw, bw, collisionBox lp, collisionBox rp]

