module Pong.Arena(
    Arena(..),
    arenaSpriteSheets,
    arena,
    arenaThink
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
        lPaddle = paddle { pName = "lPaddle", pSourcePosition = Vector 3 2, pTargetPosition = Vector 5 100},
        rPaddle = paddle { pName = "rPaddle", pSourcePosition = Vector 3 32, pTargetPosition = Vector 410 100}
}

arenaSpriteSheets :: [SpriteSheet]
arenaSpriteSheets = [ballSpriteSheet] ++ [paddleSpriteSheet] ++ [pitchSpriteSheet]

arenaThink :: Arena -> Tactile -> Arena
arenaThink a ts = a {
    lPaddle = move (lPaddle a) ts (Key_A, Key_Z),
    rPaddle = move (rPaddle a) ts (Key_Up, Key_Down)
}



