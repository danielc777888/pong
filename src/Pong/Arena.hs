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

data Arena = Arena {
    lPaddle :: Paddle,
    rPaddle :: Paddle,
    topWall :: CollisionBox,
    bottomWall :: CollisionBox
}

arena :: Arena
arena = Arena {
        Pong.Arena.lPaddle = Paddle.lPaddle { targetPosition = Vector 5 100 },
        Pong.Arena.rPaddle = Paddle.rPaddle { targetPosition = Vector 410 100 },
        topWall = CollisionBox (Vector 0 (-1)) (Vector 425 (-6)),
        bottomWall = CollisionBox (Vector 0 246) (Vector 425 241)
}

spriteSheets :: [SpriteSheet]
spriteSheets = [Ball.spriteSheet] ++ [Paddle.spriteSheet paddle] ++ [Pitch.spriteSheet]

think :: Arena -> Time -> Tactile -> Arena
think (Arena lp rp tw bw) t ts = Arena (Paddle.think lp tw bw t ts) (Paddle.think rp tw bw t ts) tw bw

getCollisionBoxes :: Arena -> [CollisionBox]
getCollisionBoxes (Arena lp rp tw bw) = [tw, bw, collisionBox lp, collisionBox rp]

