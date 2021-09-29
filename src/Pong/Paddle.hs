module Pong.Paddle
  ( Paddle (..),
    paddle,
    lPaddle,
    rPaddle,
    think,
    sprite,
    collisionBox,
  )
where

import Core.Existent (Name)
import Core.Math (Nat, Vector (..), mkVec, zeroVector)
import Core.Physics (CollisionBox (..))
import Core.Tactile (KeyboardKey (..), Tactile, touchedKey)
import Core.Time (Time (..))
import Core.Visual
  ( Animation,
    Dimensions,
    Frame (..),
    Position,
    Sprite (Sprite),
    SpriteSheet,
  )
import Data.Set

data Paddle = Paddle
  { name :: Name,
    spriteSheet :: SpriteSheet,
    sourcePosition :: Position,
    targetPosition :: Position,
    dimensions :: Dimensions,
    animation :: Animation,
    upKey :: KeyboardKey,
    downKey :: KeyboardKey,
    currentFrame :: Nat,
    currentFrameStartTime :: Double,
    speed :: Nat
  }

paddle :: Paddle
paddle = Paddle "paddle" "paddles" zeroVector zeroVector (10, 26) [] Key_Null Key_Null 0 0 200

lPaddle :: Paddle
lPaddle = paddle {name = "lPaddle", sourcePosition = Vector {vecX = 3, vecY = 2}, animation = an, upKey = Key_A, downKey = Key_Z}
  where
      an = [Frame 0 Vector{vecX = 3, vecY = 2} (10, 26) 0.1, Frame 1 Vector{vecX = 19, vecY = 2} (10, 26) 0.2, Frame 2 Vector{vecX = 35, vecY = 2} (10, 26) 0.2, Frame 3 Vector{vecX = 51, vecY = 2} (10, 26) 0.2]

rPaddle :: Paddle
rPaddle = paddle {name = "rPaddle", sourcePosition = Vector 3 32, animation = an, upKey = Key_Up, downKey = Key_Down}
  where
    an = [Frame 0 (Vector 3 32) (10, 26) 0.1, Frame 1 (Vector 19 32) (10, 26) 0.2, Frame 2 (Vector 35 32) (10, 26) 0.2, Frame 3 (Vector 51 32) (10, 26) 0.2]

collisionBox :: Paddle -> CollisionBox
collisionBox p = CollisionBox bl tr
  where
    (Vector spx spy) = targetPosition p
    (w, h) = dimensions p
    bl = Vector spx (spy + h)
    tr = Vector (spx + w) spy

collision :: CollisionBox -> CollisionBox -> Bool
collision (CollisionBox v1bl v1tr) (CollisionBox v2bl v2tr) = not test
  where
    test =
      (vecX v1tr < vecX v2bl) || (vecX v2tr < vecX v1bl)
        || (vecY v1tr > vecY v2bl)
        || (vecY v2tr > vecY v1bl)

think :: Paddle -> CollisionBox -> CollisionBox -> Time -> Tactile -> Paddle
think p tw bw t ts = a
  where
    m = move p tw bw ts (upKey p, downKey p) (universeDeltaTime t)
    a = animate m (universeTime t)

sprite :: Paddle -> Sprite
sprite p = Sprite (spriteSheet p) (fSourcePosition fr) (targetPosition p) (fDimensions fr)
  where
    fr = (animation p) !! (currentFrame p)

move :: Paddle -> CollisionBox -> CollisionBox -> Tactile -> (KeyboardKey, KeyboardKey) -> Float -> Paddle
move p tw bw ts (u, d) dt
  | touchedKey u ts = if collision (collisionBox mu) tw then moveToWall p tw else mu
  | touchedKey d ts = if collision (collisionBox md) bw then moveToWall p bw else md
  | otherwise = p
  where
    mu = p {targetPosition = moveUp (targetPosition p) dt (speed p)}
    md = p {targetPosition = moveDown (targetPosition p) dt (speed p)}

moveToWall :: Paddle -> CollisionBox -> Paddle
moveToWall p cb = p {targetPosition = Vector {vecX = (vecX tp), vecY = y'}} --mkVec (vecX tp) y'
  where
    tp = targetPosition p
    above = (vecY (bottomLeft cb)) < (vecY tp)
    (w, h) = dimensions p
    y' = if above then (vecY (bottomLeft cb)) + 1 else (vecY (topRight cb)) + (- h) + (-1)

--y' = if above then 0 else (240 - 26)

moveUp :: Position -> Float -> Nat -> Position
moveUp p dt s = Vector {vecX = (vecX p), vecY = ((vecY p) - round (fromIntegral s * dt))}

moveDown :: Position -> Float -> Nat -> Position
moveDown p dt s = Vector {vecX = (vecX p), vecY = ((vecY p) + round (fromIntegral s * dt))}

animate :: Paddle -> Double -> Paddle
animate p t = if (frElapsed p t fr) then p {currentFrame = nextFr p, currentFrameStartTime = t} else p
  where
    fr = (animation p) !! (currentFrame p)
    nextFr p = (currentFrame p + 1) `mod` (length (animation p))
    frElapsed p t fr = (t - (currentFrameStartTime p)) > (duration fr)
