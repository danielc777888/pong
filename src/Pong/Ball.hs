module Pong.Ball
  ( Ball (..),
    balBall,
    balThink,
    balSprite,
  )
where

import Core.Existent (Name)
import Core.Math (Nat, Vector (..), zeroVector)
import Core.Visual (Dimensions, Position, Sprite (Sprite), SpriteSheet)

data Ball = Ball
  { name :: Name,
    balSpriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    balTargetPosition :: Position,
    dimensions :: Dimensions,
    currentFrame :: Nat,
    currentFrameStartTime :: Double,
    speed :: Nat
  }

balBall :: Ball
balBall = Ball "ball" "ball" zeroVector zeroVector (6, 6) 0 0 200

balThink :: Ball -> Nat -> Ball
balThink b rv = b {balTargetPosition = Vector {vecX = (vecX tp), vecY = rv}}
  where
    tp = balTargetPosition b

balSprite :: Ball -> Sprite
balSprite b = Sprite (balSpriteSheet b) (sourcePosition b) (balTargetPosition b) (dimensions b)