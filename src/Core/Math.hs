module Core.Math
  ( Nat,
    Vector (..),
    mkVec,
    zeroVector,
  )
where

type Nat = Int

data Vector = Vector
  { vecX :: Int,
    vecY :: Int
  }
  deriving (Show)

zeroVector :: Vector
zeroVector = mkVec 0 0

mkVec :: Int -> Int -> Vector
mkVec x y = Vector {vecX = x, vecY = y}
