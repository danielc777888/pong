module Core.Time(
    Time(..)
) where

data Time = Time {
    realDeltaTime :: Float,
    universeDeltaTime :: Float,
    realTime :: Double
}