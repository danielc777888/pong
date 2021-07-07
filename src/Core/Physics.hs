module Core.Physics(
    CollisionBox(..)
)
 where

import Core.Math (Vector(..))

data CollisionBox  = CollisionBox {
    bottomLeft :: Vector,
    topRight :: Vector
} deriving Show