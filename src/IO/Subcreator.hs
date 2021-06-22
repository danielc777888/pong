--Reify world using raylib
module IO.Subcreator(
    subcreate
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Foreign.Ptr

import IO.Raylib

import Core.Math
import Core.Auditory
import Core.Tactile
import Core.Visual
import Core.Universe

--type synonyms
type ArtMap = (TextureMap, FontMap, SoundMap, MusicMap)
type TextureMap = M.Map String (Ptr Texture2D)
type FontMap = M.Map String (Ptr Font)
type SoundMap = M.Map String (Ptr Sound)
type MusicMap = M.Map String (Ptr Sound)

artFolder :: String
artFolder = "art/"

supportedResolutions :: [Resolution]
supportedResolutions = [(425, 240), (1024, 576), (1152, 648), (1280, 720), (1600, 900), (1920, 1080)]

--subcreate world
subcreate :: Universe a -> IO ()
subcreate u = do (u', am) <- begin u
                 exist u' am
                 end am

--begin world, init, load and setup initial state
begin :: Universe a -> IO (Universe a, ArtMap)
begin u = do    let r = resolution u
                r' <- IO.Subcreator.initWindow (name u) r supportedResolutions
                initAudioDevice
                sss <- loadSpriteSheets (spriteSheets u)
                ss <- loadSounds (sounds u)
                ms <- loadMusic (music u)
                fs <- loadFonts (fonts u)
                toggleFullScreen
                setTargetFPS (fps u)
                let u' = u { Core.Universe.scaleFactor = Core.Visual.scaleFactor r r' }
                return (u',  (sss, fs, ss, ms))

--adjust to best resolution
initWindow :: String -> Resolution -> [Resolution] -> IO Resolution
initWindow t o rs = do IO.Raylib.initWindow (fst o) (snd o) t
                       displayWidth <- getMonitorWidth 0
                       displayHeight <- getMonitorHeight 0
                       traceLog Info $ "ORIGINAL RESOLUTION " ++ show o
                       traceLog Info $ "This ROCKS"
                       traceLog Info $ "CURRENT DISPLAY " ++ show (displayWidth, displayHeight)
                       r <- if o == (displayWidth, displayHeight)
                            then do  traceLog Info $ "USE Original resolution ?" ++ show o
                                     return o
                            else do  closeWindow
                                     let r = selectResolution (displayWidth, displayHeight) rs
                                     traceLog Info $ "SELECT best resolution " ++ show r
                                     IO.Raylib.initWindow (fst r) (snd r) t
                                     return r
                       return r

--main loop for existing world
exist :: Universe a -> ArtMap -> IO ()
exist u (tm, fm, sm, mm) = do   done <- windowShouldClose
                                if not done
                                then do ts <- tactiles
                                        let u' = (Core.Universe.think u) u ts
                                        audio u' sm mm
                                        visuals u' tm fm
                                        exist u' (tm, fm, sm, mm)
                                else do extinctionLevelEvent

extinctionLevelEvent :: IO ()
extinctionLevelEvent = return ()

--unload and close everything
end ::  ArtMap -> IO ()
end (tm, fm, sm, mm) = do sequence_ [unloadTexture (snd t) | t <- M.toList tm]
                          sequence_ [unloadFont (snd f) | f <- M.toList fm]
                          sequence_ [unloadSound (snd s) | s <- M.toList sm]
                          sequence_ [unloadSound (snd s) | s <- M.toList mm]
                          closeWindow

loadSpriteSheets :: [SpriteSheet] -> IO (TextureMap)
loadSpriteSheets xs = do ts <- sequence [loadTexture (artFolder ++ "spritesheets/" ++ x ++ ".png") | x <- xs]
                         return (M.fromList (zip xs ts))

loadFonts :: [FontFile] -> IO (FontMap)
loadFonts xs = do ts <- sequence [loadFont (artFolder ++ "fonts/" ++ x ++ ".png") | x <- xs]
                  return (M.fromList (zip xs ts))

loadSounds :: [SoundFile] -> IO (SoundMap)
loadSounds xs = do ts <- mapM (\x -> loadSound (artFolder ++ "sounds/" ++ x ++ ".wav")) xs
                   return (M.fromList (zip xs ts))

loadMusic :: [MusicFile] -> IO (MusicMap)
loadMusic xs = do ts <- sequence [loadSound (artFolder ++ "music/" ++ x ++ ".ogg") | x <- xs]
                  return (M.fromList (zip xs ts))

--play all audio, sounds and music
audio :: Universe a -> SoundMap -> MusicMap -> IO ()
audio u sm mm = do sequence_ [playSound (findArt s sm) | s <- playSounds u]

visuals :: Universe a -> TextureMap -> FontMap -> IO ()
visuals u tm fm  =  do beginDrawing
                       sequence_ [drawSprite s tm (Core.Universe.scaleFactor u) | s <- drawSprites u]
                       endDrawing

drawSprite :: Sprite -> TextureMap -> (ScaleFactor, ScaleFactor) -> IO ()
drawSprite s tm sf = do drawTexturePro a sr dr (IO.Raylib.Vector2 0 0) 0.0 white
                        where d = dimensions s
                              sr = toRectangle (sourcePosition s) d (1.0, 1.0)
                              dr = toRectangle (targetPosition s) d sf
                              a = findArt (spriteSheet s) tm
--tactiles
tactiles :: IO Tactile
tactiles = do let ks = zip keyboardKeys [Core.Tactile.Key_Apostrophe .. Core.Tactile.Key_Kp_Equal]
              ksp <- keysTouched ks isKeyPressed
              ksd <- keysTouched ks isKeyDown
              return $ Tactile { Core.Tactile.keysPressed = ksp, Core.Tactile.keysDown = ksd }

keysTouched :: [(IO.Raylib.KeyboardKey, Core.Tactile.KeyboardKey)] -> (IO.Raylib.KeyboardKey -> IO Bool) -> IO (S.Set Core.Tactile.KeyboardKey)
keysTouched xs f = do ks <- sequence [do kp <- f x
                                         return (if kp then y else Core.Tactile.Key_Null) | (x, y) <- xs]
                      return (S.fromList ks)

findArt :: String -> M.Map String (Ptr a) -> Ptr a
findArt k m = fromJust (M.lookup k m)

--conversions
toRectangle :: Position -> Dimensions -> (ScaleFactor, ScaleFactor) -> IO.Raylib.Rectangle
toRectangle p (w, h) (s1, s2) = IO.Raylib.Rectangle (realToFrac x') (realToFrac y') (realToFrac w') (realToFrac h')
                                     where (w', h') = scaleDimensions (w, h) (s1, s2)
                                           (x', y') = translatePosition p (s1, s2)

--test font
drawText :: Ptr Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s p sf = do bs <- baseSize f
                       drawTextEx f s (IO.Raylib.Vector2 (fromIntegral (x p)) (fromIntegral (y p))) (fromIntegral bs * sf) sf white


--adapt positions, dimensions
translatePosition :: Position -> (ScaleFactor, ScaleFactor) -> (Float, Float)
translatePosition p (ws, hs) = (fromIntegral (x p) * ws, fromIntegral (y p) * hs)

scaleDimensions :: Dimensions -> (ScaleFactor, ScaleFactor) -> (Float, Float)
scaleDimensions (w, h) (ws, hs) = (fromIntegral w * ws, fromIntegral h * hs)

