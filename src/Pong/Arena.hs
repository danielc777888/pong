module Pong.Arena where

import Core.Visual

import Pong.Ball
import Pong.Paddle
import Pong.Pitch

spriteSheets :: [SpriteSheet]
spriteSheets = [Pong.Ball.spriteSheet] ++ [Pong.Paddle.spriteSheet] ++ [Pong.Pitch.spriteSheet]

--arena ::
--arena = score . ball . lpaddle . rpaddle . pitch