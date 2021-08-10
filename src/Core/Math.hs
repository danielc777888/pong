module Core.Math
  ( Nat,
    Vector (Vector),
    x,
    y,
    zeroVector,
  )
where

type Nat = Int

data Vector = Vector {x :: Int, y :: Int} deriving (Show)

zeroVector :: Vector
zeroVector = Vector 0 0
