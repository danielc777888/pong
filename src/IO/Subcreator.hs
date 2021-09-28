--Reify world using raylib
module IO.Subcreator
  ( subcreate,
  )
where

import Core.Auditory
import Core.Math
import Core.Tactile as Tactile
import Core.Time (Time (..))
import Core.Universe as Universe
import Core.Visual as Visual
import Data.Map (Map, toList)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Foreign.Ptr
import IO.Raylib as Raylib

--type synonyms
type ArtMap = (TextureMap, FontMap, SoundMap, MusicMap)

type TextureMap = Map String (Ptr Texture2D)

type FontMap = Map String (Ptr Font)

type SoundMap = Map String (Ptr Sound)

type MusicMap = Map String (Ptr Sound)

artFolder :: String
artFolder = "art/"

--build for 16:9 ration screens
supportedResolutions :: [Resolution]
supportedResolutions = [(425, 240), (1024, 576), (1152, 648), (1280, 720), (1600, 900), (1920, 1080)]

--supportedResolutions = [(425, 240), (1024, 576), (1152, 648), (1280, 720), (1600, 900), (1920, 1080)]

--subcreate universe
subcreate :: Universe a -> IO ()
subcreate u = do
  (u', am) <- begin u
  exist u' am
  end am

--begin world, init, load and setup initial state
begin :: Universe a -> IO (Universe a, ArtMap)
begin u = do
  let r = resolution u
  r' <- initDisplay (name u) r supportedResolutions
  initAudioDevice
  sss <- loadSpriteSheets (spriteSheets u)
  ss <- loadSounds (sounds u)
  ms <- loadMusic (music u)
  fs <- loadFonts (fonts u)
  toggleFullScreen
  setTargetFPS (fps u)
  let u' = u {Universe.scaleFactor = Visual.scaleFactor r r'}
  return (u', (sss, fs, ss, ms))

--init display with best resolution
initDisplay :: String -> Resolution -> [Resolution] -> IO Resolution
initDisplay t o rs = do
  initWindow (fst o) (snd o) t
  displayWidth <- getMonitorWidth 0
  displayHeight <- getMonitorHeight 0
  traceLog Info $ "ORIGINAL RESOLUTION " ++ show o
  traceLog Info $ "This ROCKS"
  traceLog Info $ "CURRENT DISPLAY " ++ show (displayWidth, displayHeight)
  r <-
    if o == (displayWidth, displayHeight)
      then do
        traceLog Info $ "USE Original resolution ?" ++ show o
        return o
      else do
        closeWindow
        let r = selectResolution (displayWidth, displayHeight) rs
        traceLog Info $ "SELECT best resolution " ++ show r
        initWindow (fst r) (snd r) t
        return r
  return r

--main loop for existing world
exist :: Universe a -> ArtMap -> IO ()
exist u (tm, fm, sm, mm) = do
  done <- windowShouldClose
  if not done
    then do
      t <- getTime
      --traceLog Info $ "Current time " ++ show t
      --traceLog Info $ "Collision boxes " ++ show (collisionBoxes u)
      --putStrLn ("Collision boxes " ++ show (collisionBoxes u))
      ft <- getFrameTime
      rv <- getRandomValue 100 140
      let u' = u {randomValue = rv}
      --traceLog Info $ "Current frame time " ++ show ft
      ts <- tactiles
      let u'' = (think u') u' (time t ft (timeFactor u')) ts
      audio u'' sm mm
      visuals u'' tm fm
      exist u'' (tm, fm, sm, mm)
    else do extinctionLevelEvent

extinctionLevelEvent :: IO ()
extinctionLevelEvent = return ()

time :: Double -> Float -> Float -> Time
time rt rdt tf = Time {realTime = rt, realDeltaTime = rdt, universeDeltaTime = tf * rdt, universeTime = realToFrac tf * rt}

--unload and close everything
end :: ArtMap -> IO ()
end (tm, fm, sm, mm) = do
  mapM_ (unloadTexture . snd) (toList tm)
  mapM_ (unloadFont . snd) (toList fm)
  mapM_ (unloadSound . snd) (toList sm)
  mapM_ (unloadSound . snd) (toList mm)
  closeWindow

--load art
loadSpriteSheets :: [SpriteSheet] -> IO (TextureMap)
loadSpriteSheets xs = do
  ts <- mapM (loadTexture . location) xs
  return (M.fromList (zip xs ts))
  where
    location n = artFolder ++ "spritesheets/" ++ n ++ ".png"

loadFonts :: [FontFile] -> IO (FontMap)
loadFonts xs = do
  ts <- mapM (loadFont . location) xs
  return (M.fromList (zip xs ts))
  where
    location n = artFolder ++ "fonts/" ++ n ++ ".png"

loadSounds :: [SoundFile] -> IO (SoundMap)
loadSounds xs = do
  ts <- mapM (loadSound . location) xs
  return (M.fromList (zip xs ts))
  where
    location n = artFolder ++ "sounds/" ++ n ++ ".wav"

loadMusic :: [MusicFile] -> IO (MusicMap)
loadMusic xs = do
  ts <- mapM (loadSound . location) xs
  return (M.fromList (zip xs ts))
  where
    location n = artFolder ++ "music/" ++ n ++ ".ogg"

--play all audio, sounds and music
audio :: Universe a -> SoundMap -> MusicMap -> IO ()
audio u sm mm = do mapM_ (playSound . findArt sm) (playSounds u)

visuals :: Universe a -> TextureMap -> FontMap -> IO ()
visuals u tm fm = do
  beginDrawing
  mapM_ (\s -> drawSprite s tm sf) (drawSprites u)
  endDrawing
  where
    sf = Universe.scaleFactor u

drawSprite :: Sprite -> TextureMap -> (ScaleFactor, ScaleFactor) -> IO ()
drawSprite s tm sf = do drawTexturePro a sr dr (Vector2 0 0) 0.0 white
  where
    d = dimensions s
    sr = toRectangle (sourcePosition s) d (1.0, 1.0)
    dr = toRectangle (targetPosition s) d sf
    a = findArt tm (spriteSheet s)

--tactiles
tactiles :: IO Tactile
tactiles = do
  let ks = zip keyboardKeys [Tactile.Key_Apostrophe .. Tactile.Key_Kp_Equal]
  ksp <- keysTouched ks isKeyPressed
  ksd <- keysTouched ks isKeyDown
  return $ Tactile ksp ksd

keysTouched :: [(Raylib.KeyboardKey, Tactile.KeyboardKey)] -> (Raylib.KeyboardKey -> IO Bool) -> IO (Set Tactile.KeyboardKey)
keysTouched xs f = do
  ks <-
    mapM
      ( \(x, y) -> do
          kp <- f x
          return (if kp then y else Tactile.Key_Null)
      )
      xs
  return (S.fromList ks)

findArt :: Map String (Ptr a) -> String -> Ptr a
findArt m k = fromJust (M.lookup k m)

--conversions
toRectangle :: Position -> Dimensions -> (ScaleFactor, ScaleFactor) -> Rectangle
toRectangle p (w, h) (s1, s2) = Rectangle (realToFrac x') (realToFrac y') (realToFrac w') (realToFrac h')
  where
    (w', h') = scaleDimensions (w, h) (s1, s2)
    (x', y') = translatePosition p (s1, s2)

--font
drawText :: Ptr Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s p sf = do
  bs <- baseSize f
  drawTextEx f s (Vector2 (fromIntegral (vecX p)) (fromIntegral (vecY p))) (fromIntegral bs * sf) sf white

--adapt positions, dimensions
translatePosition :: Position -> (ScaleFactor, ScaleFactor) -> (Float, Float)
translatePosition p (ws, hs) = (fromIntegral (vecX p) * ws, fromIntegral (vecY p) * hs)

scaleDimensions :: Dimensions -> (ScaleFactor, ScaleFactor) -> (Float, Float)
scaleDimensions (w, h) (ws, hs) = (fromIntegral w * ws, fromIntegral h * hs)