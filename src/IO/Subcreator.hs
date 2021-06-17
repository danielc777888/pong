--Reify world using raylib
--must not depend on pong stuff
module IO.Subcreator where

import qualified Data.Map as M
import Data.Maybe
import Foreign.Ptr
import qualified IO.Raylib as R
import Core.Math
import Core.Visual
import Core.Universe
import qualified Pong.World as W
import Pong.Universe

--type synonyms
type ArtMap = (TextureMap, FontMap, SoundMap, MusicMap)
type TextureMap = M.Map String (Ptr R.Texture2D)
type FontMap = M.Map String (Ptr R.Font)
type SoundMap = M.Map String (Ptr R.Sound)
type MusicMap = M.Map String (Ptr R.Sound)

artFolder :: String
artFolder = "art/"

fps :: Nat
fps = 12

supportedResolutions :: [Resolution]
supportedResolutions = [(425, 240), (1024, 576), (1152, 648), (1280, 720), (1600, 900), (1920, 1080)]

--subcreate world
subcreate :: W.World -> Universe -> IO ()
subcreate w u = do (w', am) <- begin w u
                   exist w' am
                   end am

--begin world, init, load and setup initial state
begin :: W.World -> Universe -> IO (W.World, ArtMap)
begin w u = do  r <- IO.Subcreator.initWindow (name u) (resolution u) supportedResolutions
                R.initAudioDevice
                sss <- loadSpriteSheets (spriteSheets u)
                ss <- loadSounds (sounds u)
                ms <- loadMusic (music u)
                fs <- loadFonts (fonts u)
                R.toggleFullScreen
                R.setTargetFPS fps
                let w' = W.changeResolution w r
                return (w', (sss, fs, ss, ms))

--adjust to best resolution
initWindow :: String -> Resolution -> [Resolution] -> IO Resolution
initWindow t o rs = do R.initWindow (fst o) (snd o) t
                       displayWidth <- R.getMonitorWidth 0
                       displayHeight <- R.getMonitorHeight 0
                       R.traceLog R.Info $ "ORIGINAL RESOLUTION " ++ show o
                       R.traceLog R.Info $ "CURRENT DISPLAY " ++ show (displayWidth, displayHeight)
                       r <- if o == (displayWidth, displayHeight)
                            then do  R.traceLog R.Info $ "USE Original resolution " ++ show o
                                     return o
                            else do  R.closeWindow
                                     let r = selectResolution (displayWidth, displayHeight) rs
                                     R.traceLog R.Info $ "SELECT best resolution " ++ show r
                                     R.initWindow (fst r) (snd r) t
                                     return r
                       return r

--main loop for existing world
exist :: W.World -> ArtMap -> IO ()
exist w (tm, fm, sm, mm) = do done <- R.windowShouldClose
                              if not done
                              then do ts <- tactiles
                                      let w' = W.think w ts
                                      audio w' ts sm mm
                                      visuals w' ts tm fm
                                      exist w' (tm, fm, sm, mm)
                              else do extinctionLevelEvent

extinctionLevelEvent :: IO ()
extinctionLevelEvent = return ()

--unload and close everything
end ::  ArtMap -> IO ()
end (tm, fm, sm, mm) = do sequence_ [R.unloadTexture (snd t) | t <- M.toList tm]
                          sequence_ [R.unloadFont (snd f) | f <- M.toList fm]
                          sequence_ [R.unloadSound (snd s) | s <- M.toList sm]
                          sequence_ [R.unloadSound (snd s) | s <- M.toList mm]
                          R.closeWindow

loadSpriteSheets :: [SpriteSheet] -> IO (TextureMap)
loadSpriteSheets xs = do ts <- sequence [R.loadTexture (artFolder ++ "spritesheets/" ++ x ++ ".png") | x <- xs]
                         return (M.fromList (zip xs ts))

loadFonts :: [FontFile] -> IO (FontMap)
loadFonts xs = do ts <- sequence [R.loadFont (artFolder ++ "fonts/" ++ x ++ ".png") | x <- xs]
                  return (M.fromList (zip xs ts))

loadSounds :: [SoundFile] -> IO (SoundMap)
loadSounds xs = do ts <- sequence [R.loadSound (artFolder ++ "sounds/" ++ x ++ ".wav") | x <- xs]
                   return (M.fromList (zip xs ts))

loadMusic :: [MusicFile] -> IO (MusicMap)
loadMusic xs = do ts <- sequence [R.loadSound (artFolder ++ "music/" ++ x ++ ".ogg") | x <- xs]
                  return (M.fromList (zip xs ts))

--play all audio, sounds and music
audio :: W.World -> [W.Act] -> SoundMap -> MusicMap -> IO ()
audio w as sm mm = do soundPlayer (W.player w) as sm

soundPlayer :: W.Player -> [W.Act] -> SoundMap -> IO ()
soundPlayer p as sm = do let j = W.Jump `elem` as
                         if j then R.playSound (findArt "sound" sm) else return ()

--draw allW. visuals
visuals :: W.World -> [W.Act] -> TextureMap -> FontMap -> IO ()
visuals w as tm fm  =  do R.beginDrawing
                          drawBackground (W.background w) tm
                          drawPlayer (W.player w) tm fm
                          R.endDrawing

drawBackground :: W.Background -> TextureMap -> IO ()
drawBackground (W.Background p sf) tm = do  --R.traceLog R.Info $ "BACKGROUND scale factor " ++ show sf
                                            --R.traceLog R.Info $ "BACKGROUND position " ++ show p
                                            --drawTexture (findArt "pitch" tm) p (snd sf)
                                            R.drawTexturePro (findArt "pitch" tm) (R.Rectangle 0 0 425 240) (R.Rectangle (fromIntegral 0) (fromIntegral 0) x' y') (R.Vector2 0 0) 0.0 R.white
                                             where x' = realToFrac (425 * (fst sf))
                                                   y' = realToFrac (240 * (snd sf))

drawPlayer :: W.Player -> TextureMap -> FontMap -> IO ()
drawPlayer p tm fm = do drawTexture' (findArt "paddle" tm) (W.pRect p) (W.pPosition p) (W.pScaleFactor p)
                                              --drawText (findArt f fm) "This is my first text" p s

--actions of player
--TODO: There must be a more elegant way to do this
tactiles :: IO [W.Act]
tactiles = do s <- R.isKeyPressed R.Space
              lc <- R.isKeyPressed R.LControl
              return $ (if s then [W.Jump] else []) ++ (if lc then [W.Attack] else [])

findArt :: String -> M.Map String (Ptr a) -> Ptr a
findArt k m = fromJust (M.lookup k m)

drawTexture :: Ptr R.Texture2D -> Position -> ScaleFactor -> IO ()
drawTexture t (x, y) s = do R.drawTextureEx t (R.Vector2 (fromIntegral x) (fromIntegral y)) 0.0 s R.white

drawTexture' :: Ptr R.Texture2D -> W.Rectangle -> Position -> (ScaleFactor, ScaleFactor) -> IO ()
drawTexture' t r (x, y) (w, h) = do R.drawTexturePro t (R.Rectangle 0 0 10 26) (R.Rectangle (fromIntegral x) (fromIntegral y) x' y') (R.Vector2 0 0) 0.0 R.white
                                   where x' = realToFrac (10 * w)
                                         y' = realToFrac (26 * h)

fromRec :: W.Rectangle -> R.Rectangle
fromRec (W.Rectangle (x, y) (x', y')) = R.Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')


drawText :: Ptr R.Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s (x, y) sf = do bs <- R.baseSize f
                            R.drawTextEx f s (R.Vector2 (fromIntegral x) (fromIntegral y)) (fromIntegral bs * sf) sf R.white


