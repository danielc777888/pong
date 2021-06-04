--Reify world using raylib
module Subcreator where

import qualified Data.Map as M
import Data.Maybe
import Foreign.Ptr
import Raylib
import World

--type synonyms
type ArtMap = (TextureMap, FontMap, SoundMap, MusicMap)
type TextureMap = M.Map String (Ptr Texture2D)
type FontMap = M.Map String (Ptr Font)
type SoundMap = M.Map String (Ptr Sound)
type MusicMap = M.Map String (Ptr Sound)

artFolder :: String
artFolder = "art/"

--subcreate world
subcreate :: World -> IO ()
subcreate w = do (w', am) <- begin w
                 exist w' am
                 end am

--begin world, init, load and setup initial state
begin :: World -> IO (World, ArtMap)
begin w = do r <- Subcreator.initWindow title originalResolution supportedResolutions
             initAudioDevice
             sss <- loadSpriteSheets (spriteSheets w)
             ss <- loadSounds (soundFiles w)
             ms <- loadMusic (musicFiles w)
             fs <- loadFonts (fontFiles w)
             toggleFullScreen
             setTargetFPS fps
             let w' = changeResolution w r
             return (w', (sss, fs, ss, ms))

--adjust to best resolution
initWindow :: String -> Resolution -> [Resolution] -> IO Resolution
initWindow t o rs = do Raylib.initWindow (fst o) (snd o) t
                       displayWidth <- getMonitorWidth 0
                       displayHeight <- getMonitorHeight 0
                       traceLog Info $ "ORIGINAL RESOLUTION " ++ show o
                       traceLog Info $ "CURRENT DISPLAY " ++ show (displayWidth, displayHeight)
                       r <- if o == (displayWidth, displayHeight)
                            then do  traceLog Info $ "USE Original resolution " ++ show o
                                     return o
                            else do  closeWindow
                                     let r = selectResolution (displayWidth, displayHeight) rs
                                     traceLog Info $ "SELECT best resolution " ++ show r
                                     Raylib.initWindow (fst r) (snd r) t
                                     return r
                       return r

--main loop for existing world
exist :: World -> ArtMap -> IO ()
exist w (tm, fm, sm, mm) = do done <- windowShouldClose
                              if not done
                              then do as <- acts
                                      let w' = think w as
                                      audio w' as sm mm
                                      visual w' as tm fm
                                      exist w' (tm, fm, sm, mm)
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
audio :: World -> [Act] -> SoundMap -> MusicMap -> IO ()
audio w as sm mm = do soundPlayer (player w) as sm

soundPlayer :: Player -> [Act] -> SoundMap -> IO ()
soundPlayer p as sm = do let j = Jump `elem` as
                         if j then playSound (findArt "sound" sm) else return ()

--draw all visuals
visual :: World -> [Act] -> TextureMap -> FontMap -> IO ()
visual w as tm fm  =  do beginDrawing
                         drawBackground (background w) tm
                         drawPlayer (player w) tm fm
                         endDrawing

drawBackground :: Background -> TextureMap -> IO ()
drawBackground (Background p sf) tm = do drawTexture (findArt "background_1" tm) p sf

drawPlayer :: Player -> TextureMap -> FontMap -> IO ()
drawPlayer p tm fm = do drawTexture' (findArt "shep" tm) (pRect p) (pPosition p) (pScaleFactor p)
                                              --drawText (findArt f fm) "This is my first text" p s

--actions of player
--TODO: There must be a more elegant way to do this
acts :: IO [Act]
acts = do s <- isKeyPressed Space
          lc <- isKeyPressed LControl
          return $ (if s then [Jump] else []) ++ (if lc then [Attack] else [])

findArt :: String -> M.Map String (Ptr a) -> Ptr a
findArt k m = fromJust (M.lookup k m)

drawTexture :: Ptr Texture2D -> Position -> ScaleFactor -> IO ()
drawTexture t (x, y) s = do drawTextureEx t (Vector2 (fromIntegral x) (fromIntegral y)) 0.0 s white

drawTexture' :: Ptr Texture2D -> World.Rectangle -> Position -> ScaleFactor -> IO ()
drawTexture' t r (x, y) s = do drawTexturePro t (Raylib.Rectangle 0 0 40 40) (Raylib.Rectangle (fromIntegral x) (fromIntegral y) x' y') (Vector2 0 0) 0.0 white
                                   where x' = realToFrac (40 * s)
                                         y' = realToFrac (40 * s)

fromRec :: World.Rectangle -> Raylib.Rectangle
fromRec (World.Rectangle (x, y) (x', y')) = Raylib.Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')


drawText :: Ptr Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s (x, y) sf = do bs <- baseSize f
                            drawTextEx f s (Vector2 (fromIntegral x) (fromIntegral y)) (fromIntegral bs * sf) sf white


