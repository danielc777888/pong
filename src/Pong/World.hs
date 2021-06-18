--Description of world
--No knowledge of Raylib in here, just be able to describe world
module Pong.World where

import Core.Visual
import Core.Auditory
import Core.Tactile
import Core.Universe


--root, the singularity
data World = World 
    { resolution :: Resolution 
    , wScaleFactor :: (ScaleFactor, ScaleFactor)
    , spriteSheets :: [SpriteSheet]
    , fontFiles :: [FontFile]
    , soundFiles :: [SoundFile]
    , musicFiles :: [MusicFile]
    , player :: Player
    , background :: Background 
    }

data Player = Player 
    { pPosition :: Position
    , pScaleFactor :: (ScaleFactor, ScaleFactor)
    , pState :: PlayerState
    , pRect :: Rectangle
    }

data Background = Background Position (ScaleFactor, ScaleFactor)

data PlayerState = Idle
                 | Jumping
                 | Attacking

data Act = Jump
         | Attack deriving (Eq, Show)

data Rectangle = Rectangle (Int, Int) (Int, Int)

--global constants
title :: String
title = "PONG"


--support 16:9 widescreen resolutions
originalResolution :: Resolution
originalResolution = (425, 240)

--initial world setup
world :: World
world = World { Pong.World.resolution = originalResolution
    , wScaleFactor = (1.0, 1.0)
    , Pong.World.spriteSheets = ["ball","paddle", "pitch", "start"]
    , fontFiles = ["alagard"]
    , soundFiles = ["sound"]
    , musicFiles =  ["target"]
    , player = player
    , background = background }
 where player = Player {pPosition = (10, 100), pScaleFactor = (1.0, 1.0),  pState = Idle, pRect = (Rectangle (0,0) (10, 26))}
       background = Background (0, 0) (1.0, 1.0)
        
--thinking section, changing of world
think :: World -> [Tactile] -> World
think w ts = w {player = p'}
             where p' = thinkPlayer (player w) ts

thinkPlayer :: Player -> [Tactile] -> Player
thinkPlayer p ts = if Core.Tactile.Space `elem` ts then p {pState = Jumping} else p

changeResolution :: World -> Resolution -> World
changeResolution w r' = if (originalResolution == r') then w  else w { Pong.World.resolution = r', wScaleFactor = sf', player = p', background = b' }
                                       where sf' = Core.Visual.scaleFactor (Pong.World.resolution w) r'
                                             p' = scalePlayer (player w) sf'
                                             b' = scaleBackground (background w) sf'
                                        
scalePlayer :: Player -> (ScaleFactor, ScaleFactor) -> Player
scalePlayer p sf' = p { pPosition = position, pScaleFactor = sf' }
            where position = translatePosition (pPosition p) sf'

scaleBackground :: Background -> (ScaleFactor, ScaleFactor) -> Background
scaleBackground (Background p sf) sf' = Background position sf'
                        where position = translatePosition p sf'

