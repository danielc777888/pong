module Pong.Arena
  ( Arena (..),
    arnSpriteSheets,
    arena,
    arnThink,
    getCollisionBoxes,
  )
where

import Core.Math
import Core.Physics (CollisionBox (..))
import Core.Tactile
import Core.Time (Time (..))
import Core.Visual (SpriteSheet)
import Data.Maybe
import Pong.Ball
import Pong.Paddle (Paddle (..), collisionBox, paddle, pdlLPaddle, pdlRPaddle, pdlSpriteSheet, pdlThink)
import Pong.Pitch

data Arena = Arena
  { lPaddle :: Paddle,
    rPaddle :: Paddle,
    topWall :: CollisionBox,
    bottomWall :: CollisionBox,
    ball :: Ball
  }

arena :: Arena
arena =
  Arena
    { Pong.Arena.lPaddle = pdlLPaddle {pdlTargetPosition = Vector {vecX = 5, vecY = 100}},
      Pong.Arena.rPaddle = pdlRPaddle {pdlTargetPosition = Vector 410 100},
      topWall = CollisionBox (Vector 0 (-1)) (Vector 425 (-6)),
      bottomWall = CollisionBox (Vector 0 246) (Vector 425 241),
      Pong.Arena.ball = balBall {balTargetPosition = Vector 212 120}
    }

arnSpriteSheets :: [SpriteSheet]
arnSpriteSheets = [balSpriteSheet balBall] ++ [pdlSpriteSheet paddle] ++ [pthSpriteSheet]

arnThink :: Arena -> Time -> Tactile -> Maybe Nat -> Arena
arnThink (Arena lp rp tw bw b) t ts rv = Arena (pdlThink lp tw bw t ts) (pdlThink rp tw bw t ts) tw bw (balThink b rv)

getCollisionBoxes :: Arena -> [CollisionBox]
getCollisionBoxes (Arena lp rp tw bw b) = [tw, bw, collisionBox lp, collisionBox rp]
