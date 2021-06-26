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
        lPaddle = paddle { pName = "lPaddle", pSourcePosition = Vector {x = 3, y = 2}, pTargetPosition = Vector {x = 5, y = 100}},
        rPaddle = paddle { pName = "rPaddle", pSourcePosition = Vector {x = 3, y = 32}, pTargetPosition = Vector {x = 410, y = 100}}
}

arenaSpriteSheets :: [SpriteSheet]
arenaSpriteSheets = [ballSpriteSheet] ++ [paddleSpriteSheet] ++ [pitchSpriteSheet]

arenaThink :: Arena -> Tactile -> Arena
arenaThink a ts = a {
    lPaddle = move (lPaddle a) ts (Key_A, Key_Z),
    rPaddle = move (rPaddle a) ts (Key_Up, Key_Down)
}



