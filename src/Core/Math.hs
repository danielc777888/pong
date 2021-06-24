module Core.Math(
    Nat,
    Vector(..),
    zeroVector
) where

type Nat = Int

data Vector = Vector {
    x :: Int,
    y :: Int
}

zeroVector :: Vector
zeroVector = Vector {x = 0, y = 0}