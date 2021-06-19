--Reify world using raylib
--must not depend on pong stuff
module IO.Subcreator where

import qualified Data.Map as M
import Data.Maybe
import Foreign.Ptr

--import qualified IO.Raylib as R
import IO.Raylib

import Core.Math
import Core.Auditory
import Core.Tactile
import Core.Visual
import Core.Universe

import qualified Pong.World as W
import Pong.Universe

--type synonyms
type ArtMap = (TextureMap, FontMap, SoundMap, MusicMap)
type TextureMap = M.Map String (Ptr Texture2D)
type FontMap = M.Map String (Ptr Font)
type SoundMap = M.Map String (Ptr Sound)
type MusicMap = M.Map String (Ptr Sound)

artFolder :: String
artFolder = "art/"

fps :: Nat
fps = 12

supportedResolutions :: [Resolution]
supportedResolutions = [(425, 240), (1024, 576), (1152, 648), (1280, 720), (1600, 900), (1920, 1080)]

--subcreate world
subcreate :: W.World -> Universe -> IO ()
subcreate w u = do (w', u', am) <- begin w u
                   exist w' u' am
                   end am

--begin world, init, load and setup initial state
begin :: W.World -> Universe -> IO (W.World, Universe, ArtMap)
begin w u = do  let r = resolution u
                r' <- IO.Subcreator.initWindow (name u) r supportedResolutions
                initAudioDevice
                sss <- loadSpriteSheets (spriteSheets u)
                ss <- loadSounds (sounds u)
                ms <- loadMusic (music u)
                fs <- loadFonts (fonts u)
                toggleFullScreen
                setTargetFPS fps
                let w' = W.changeResolution w r'
                let u' = u { Core.Universe.scaleFactor = Core.Visual.scaleFactor r r' }
                return (w', u',  (sss, fs, ss, ms))

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
exist :: W.World -> Universe -> ArtMap -> IO ()
exist w u (tm, fm, sm, mm) = do done <- windowShouldClose
                                if not done
                                then do ts <- tactiles
                                        let w' = W.think w ts
                                        let u' = (Core.Universe.think u) u ts
                                        audio w' ts sm mm
                                        visuals w' ts tm fm
                                        exist w' u (tm, fm, sm, mm)
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
loadSounds xs = do ts <- sequence [loadSound (artFolder ++ "sounds/" ++ x ++ ".wav") | x <- xs]
                   return (M.fromList (zip xs ts))

loadMusic :: [MusicFile] -> IO (MusicMap)
loadMusic xs = do ts <- sequence [loadSound (artFolder ++ "music/" ++ x ++ ".ogg") | x <- xs]
                  return (M.fromList (zip xs ts))

--play all audio, sounds and music
audio :: W.World -> [Tactile] -> SoundMap -> MusicMap -> IO ()
audio w ts sm mm = do soundPlayer (W.player w) ts sm

soundPlayer :: W.Player -> [Tactile] -> SoundMap -> IO ()
soundPlayer p ts sm = do let j = Core.Tactile.Space `elem` ts
                         if j then playSound (findArt "sound" sm) else return ()

--draw allW. visuals
visuals :: W.World -> [Tactile] -> TextureMap -> FontMap -> IO ()
visuals w ts tm fm  =  do beginDrawing
                          drawBackground (W.background w) tm
                          drawPlayer (W.player w) tm fm
                          endDrawing

drawBackground :: W.Background -> TextureMap -> IO ()
drawBackground (W.Background p sf) tm = do  --R.traceLog R.Info $ "BACKGROUND scale factor " ++ show sf
                                            --R.traceLog R.Info $ "BACKGROUND position " ++ show p
                                            --drawTexture (findArt "pitch" tm) p (snd sf)
                                             drawTexturePro (findArt "pitch" tm) (IO.Raylib.Rectangle 0 0 425 240) (IO.Raylib.Rectangle (fromIntegral 0) (fromIntegral 0) x' y') (IO.Raylib.Vector2 0 0) 0.0 white
                                             where x' = realToFrac (425 * (fst sf))
                                                   y' = realToFrac (240 * (snd sf))

drawPlayer :: W.Player -> TextureMap -> FontMap -> IO ()
drawPlayer p tm fm = do drawTexture' (findArt "paddle" tm) (W.pRect p) (W.pPosition p) (W.pScaleFactor p)
                                              --drawText (findArt f fm) "This is my first text" p s

--actions of player
--TODO: There must be a more elegant way to do this
--TODO: Should be a Set data structure
tactiles :: IO [Tactile]
tactiles = do s <- isKeyPressed IO.Raylib.Space
              lc <- isKeyPressed IO.Raylib.LControl
              return $ (if s then [Core.Tactile.Space] else []) ++ (if lc then [Core.Tactile.LControl] else [])

findArt :: String -> M.Map String (Ptr a) -> Ptr a
findArt k m = fromJust (M.lookup k m)

drawTexture :: Ptr Texture2D -> Position -> ScaleFactor -> IO ()
drawTexture t (x, y) s = do drawTextureEx t (Vector2 (fromIntegral x) (fromIntegral y)) 0.0 s white

drawTexture' :: Ptr Texture2D -> W.Rectangle -> Position -> (ScaleFactor, ScaleFactor) -> IO ()
drawTexture' t r (x, y) (w, h) = do IO.Raylib.drawTexturePro t (IO.Raylib.Rectangle 0 0 10 26) (IO.Raylib.Rectangle (fromIntegral x) (fromIntegral y) x' y') (Vector2 0 0) 0.0 white
                                   where x' = realToFrac (10 * w)
                                         y' = realToFrac (26 * h)

fromRec :: W.Rectangle -> IO.Raylib.Rectangle
fromRec (W.Rectangle (x, y) (x', y')) = IO.Raylib.Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')


drawText :: Ptr Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s (x, y) sf = do bs <- baseSize f
                            drawTextEx f s (IO.Raylib.Vector2 (fromIntegral x) (fromIntegral y)) (fromIntegral bs * sf) sf white


