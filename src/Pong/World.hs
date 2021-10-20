module Pong.World
  ( universe,
  )
where

import Core.Auditory
import Core.Math
import Core.Tactile
import Core.Time (Time (..))
import Core.Universe
import Core.Visual (Sprite)
import qualified Data.Set as S
import Pong.Arena
import Pong.Ball (balSprite)
import Pong.Paddle (pdlSprite)
import Pong.Pitch
import Pong.Start

newtype World = World {wldArena :: Arena}

universe :: Universe World
universe =
  Universe
    { name = "pong",
      fps = 24,
      resolution = (425, 240),
      adaptedResolution = (425, 240),
      uniScaleFactor = (1.0, 1.0),
      uniSpriteSheets = arnSpriteSheets ++ [strSpriteSheet],
      fonts = ["alagard"],
      sounds = ["sound"],
      music = ["target"],
      uniThink = Pong.World.think,
      playSounds = [],
      drawSprites = [],
      timeFactor = 1.0,
      collisionBoxes = [],
      randomValues = [((50, 190), 0)],
      worlds = World arena
    }

think :: Universe World -> Time -> Tactile -> Universe World
think u t ts =
  u
    { playSounds = soundsToPlay ts,
      drawSprites = spritesToDraw w,
      collisionBoxes = getCollisionBoxes a,
      worlds = w,
      randomValues = []
    }
  where
    a = Pong.World.wldArena (worlds u)
    rvs = randomValues u
    rv = if null rvs then Nothing else Just (snd ((randomValues u) !! 0))
    w = World (arnThink a t ts rv)

soundsToPlay :: Tactile -> [SoundFile]
soundsToPlay ts = if S.member Key_Space (keysPressed ts) then ["sound"] else []

spritesToDraw :: World -> [Sprite]
spritesToDraw (World a) = [pthSprite, pdlSprite (lPaddle a), pdlSprite (rPaddle a), balSprite (ball a)]
