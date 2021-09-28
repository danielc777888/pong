module Core.Math
  ( Nat,
    Vector (..),
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
zeroVector = Vector {vecX = 0, vecY = 0}
