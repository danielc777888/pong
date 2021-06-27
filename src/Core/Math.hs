module Core.Math(
    Nat,
    Vector(Vector), X, Y,
    x, altx, y, alty,
    zeroVector
) where

type Nat = Int
type X = Int
type Y = Int

data Vector = Vector X Y

--selectors/alters
x :: Vector -> X
x (Vector x _) = x

altx :: Vector -> X -> Vector
altx (Vector _ y) x = Vector x y

y :: Vector -> Y
y (Vector _ y) = y

alty :: Vector -> Y -> Vector
alty (Vector x _) y = Vector x y

zeroVector :: Vector
zeroVector = Vector 0 0
