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
loadSounds xs = do ts <- sequence [loadSound (artFolder ++ "sounds/" ++ x ++ ".wav") | x <- xs]
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
tactiles = do let ks = keyboardKeys
              ksp <- keysTouched ks isKeyPressed
              ksd <- keysTouched ks isKeyDown
              return $ Tactile { Core.Tactile.keysPressed = ksp, Core.Tactile.keysDown = ksd }

keysTouched :: [IO.Raylib.KeyboardKey] -> (IO.Raylib.KeyboardKey -> IO Bool) -> IO (S.Set Core.Tactile.KeyboardKey)
keysTouched xs f = do ks <- sequence [do kp <- f x
                                         return (if kp then toKeyboardKey x else Core.Tactile.Key_Null) | x <- xs]
                      return (S.fromList ks)

--TODO: Deal with boilerplate
toKeyboardKey :: IO.Raylib.KeyboardKey -> Core.Tactile.KeyboardKey
toKeyboardKey k
    | k == IO.Raylib.Key_Apostrophe = Core.Tactile.Key_Apostrophe
    | k == IO.Raylib.Key_Comma = Core.Tactile.Key_Comma
    | k == IO.Raylib.Key_Minus = Core.Tactile.Key_Minus
    | k == IO.Raylib.Key_Period = Core.Tactile.Key_Period
    | k == IO.Raylib.Key_Slash = Core.Tactile.Key_Slash
    | k == IO.Raylib.Key_Zero = Core.Tactile.Key_Zero
    | k == IO.Raylib.Key_One = Core.Tactile.Key_One
    | k == IO.Raylib.Key_Two = Core.Tactile.Key_Two
    | k == IO.Raylib.Key_Three = Core.Tactile.Key_Three
    | k == IO.Raylib.Key_Four = Core.Tactile.Key_Four
    | k == IO.Raylib.Key_Five = Core.Tactile.Key_Five
    | k == IO.Raylib.Key_Six = Core.Tactile.Key_Six
    | k == IO.Raylib.Key_Seven = Core.Tactile.Key_Seven
    | k == IO.Raylib.Key_Eight = Core.Tactile.Key_Eight
    | k == IO.Raylib.Key_Nine = Core.Tactile.Key_Nine
    | k == IO.Raylib.Key_Semicolon = Core.Tactile.Key_Semicolon
    | k == IO.Raylib.Key_Equal = Core.Tactile.Key_Equal
    | k == IO.Raylib.Key_A = Core.Tactile.Key_A
    | k == IO.Raylib.Key_B = Core.Tactile.Key_B
    | k == IO.Raylib.Key_C = Core.Tactile.Key_C
    | k == IO.Raylib.Key_D = Core.Tactile.Key_D
    | k == IO.Raylib.Key_E = Core.Tactile.Key_E
    | k == IO.Raylib.Key_F = Core.Tactile.Key_F
    | k == IO.Raylib.Key_G = Core.Tactile.Key_G
    | k == IO.Raylib.Key_H = Core.Tactile.Key_H
    | k == IO.Raylib.Key_I = Core.Tactile.Key_I
    | k == IO.Raylib.Key_J = Core.Tactile.Key_J
    | k == IO.Raylib.Key_K = Core.Tactile.Key_K
    | k == IO.Raylib.Key_L = Core.Tactile.Key_L
    | k == IO.Raylib.Key_M = Core.Tactile.Key_M
    | k == IO.Raylib.Key_N = Core.Tactile.Key_N
    | k == IO.Raylib.Key_O = Core.Tactile.Key_O
    | k == IO.Raylib.Key_P = Core.Tactile.Key_P
    | k == IO.Raylib.Key_Q = Core.Tactile.Key_Q
    | k == IO.Raylib.Key_R = Core.Tactile.Key_R
    | k == IO.Raylib.Key_S = Core.Tactile.Key_S
    | k == IO.Raylib.Key_T = Core.Tactile.Key_T
    | k == IO.Raylib.Key_U = Core.Tactile.Key_U
    | k == IO.Raylib.Key_V = Core.Tactile.Key_V
    | k == IO.Raylib.Key_W = Core.Tactile.Key_W
    | k == IO.Raylib.Key_X = Core.Tactile.Key_X
    | k == IO.Raylib.Key_Y = Core.Tactile.Key_Y
    | k == IO.Raylib.Key_Z = Core.Tactile.Key_Z
    | k == IO.Raylib.Key_Space = Core.Tactile.Key_Space
    | k == IO.Raylib.Key_Escape = Core.Tactile.Key_Escape
    | k == IO.Raylib.Key_Enter = Core.Tactile.Key_Enter
    | k == IO.Raylib.Key_Tab = Core.Tactile.Key_Tab
    | k == IO.Raylib.Key_Backspace = Core.Tactile.Key_Backspace
    | k == IO.Raylib.Key_Insert = Core.Tactile.Key_Insert
    | k == IO.Raylib.Key_Delete = Core.Tactile.Key_Delete
    | k == IO.Raylib.Key_Right = Core.Tactile.Key_Right
    | k == IO.Raylib.Key_Left = Core.Tactile.Key_Left
    | k == IO.Raylib.Key_Down = Core.Tactile.Key_Down
    | k == IO.Raylib.Key_Up = Core.Tactile.Key_Up
    | k == IO.Raylib.Key_Page_Up = Core.Tactile.Key_Page_Up
    | k == IO.Raylib.Key_Page_Down = Core.Tactile.Key_Page_Down
    | k == IO.Raylib.Key_Home = Core.Tactile.Key_Home
    | k == IO.Raylib.Key_End = Core.Tactile.Key_End
    | k == IO.Raylib.Key_Caps_Lock = Core.Tactile.Key_Caps_Lock
    | k == IO.Raylib.Key_Scroll_Lock = Core.Tactile.Key_Scroll_Lock
    | k == IO.Raylib.Key_Num_Lock = Core.Tactile.Key_Num_Lock
    | k == IO.Raylib.Key_Print_Screen = Core.Tactile.Key_Print_Screen
    | k == IO.Raylib.Key_Pause = Core.Tactile.Key_Pause
    | k == IO.Raylib.Key_F1 = Core.Tactile.Key_F1
    | k == IO.Raylib.Key_F2 = Core.Tactile.Key_F2
    | k == IO.Raylib.Key_F3 = Core.Tactile.Key_F3
    | k == IO.Raylib.Key_F4 = Core.Tactile.Key_F4
    | k == IO.Raylib.Key_F5 = Core.Tactile.Key_F5
    | k == IO.Raylib.Key_F6 = Core.Tactile.Key_F6
    | k == IO.Raylib.Key_F7 = Core.Tactile.Key_F7
    | k == IO.Raylib.Key_F8 = Core.Tactile.Key_F8
    | k == IO.Raylib.Key_F9 = Core.Tactile.Key_F9
    | k == IO.Raylib.Key_F10 = Core.Tactile.Key_F10
    | k == IO.Raylib.Key_F11 = Core.Tactile.Key_F11
    | k == IO.Raylib.Key_F12 = Core.Tactile.Key_F12
    | k == IO.Raylib.Key_Left_Shift = Core.Tactile.Key_Left_Shift
    | k == IO.Raylib.Key_Left_Control = Core.Tactile.Key_Left_Control
    | k == IO.Raylib.Key_Left_Alt = Core.Tactile.Key_Left_Alt
    | k == IO.Raylib.Key_Left_Super = Core.Tactile.Key_Left_Super
    | k == IO.Raylib.Key_Right_Shift = Core.Tactile.Key_Right_Shift
    | k == IO.Raylib.Key_Right_Control = Core.Tactile.Key_Right_Control
    | k == IO.Raylib.Key_Right_Alt = Core.Tactile.Key_Right_Alt
    | k == IO.Raylib.Key_Right_Super = Core.Tactile.Key_Right_Super
    | k == IO.Raylib.Key_Kb_Menu = Core.Tactile.Key_Kb_Menu
    | k == IO.Raylib.Key_Right_Super = Core.Tactile.Key_Right_Super
    | k == IO.Raylib.Key_Left_Bracket = Core.Tactile.Key_Left_Bracket
    | k == IO.Raylib.Key_Backslash = Core.Tactile.Key_Backslash
    | k == IO.Raylib.Key_Right_Bracket = Core.Tactile.Key_Right_Bracket
    | k == IO.Raylib.Key_Grave = Core.Tactile.Key_Grave
    | k == IO.Raylib.Key_Kp_0 = Core.Tactile.Key_Kp_0
    | k == IO.Raylib.Key_Kp_1 = Core.Tactile.Key_Kp_1
    | k == IO.Raylib.Key_Kp_2 = Core.Tactile.Key_Kp_2
    | k == IO.Raylib.Key_Kp_3 = Core.Tactile.Key_Kp_3
    | k == IO.Raylib.Key_Kp_4 = Core.Tactile.Key_Kp_4
    | k == IO.Raylib.Key_Kp_5 = Core.Tactile.Key_Kp_5
    | k == IO.Raylib.Key_Kp_6 = Core.Tactile.Key_Kp_6
    | k == IO.Raylib.Key_Kp_7 = Core.Tactile.Key_Kp_7
    | k == IO.Raylib.Key_Kp_8 = Core.Tactile.Key_Kp_8
    | k == IO.Raylib.Key_Kp_9 = Core.Tactile.Key_Kp_9
    | k == IO.Raylib.Key_Kp_Decimal = Core.Tactile.Key_Kp_Decimal
    | k == IO.Raylib.Key_Kp_Divide = Core.Tactile.Key_Kp_Divide
    | k == IO.Raylib.Key_Kp_Multiply = Core.Tactile.Key_Kp_Multiply
    | k == IO.Raylib.Key_Kp_Subtract = Core.Tactile.Key_Kp_Subtract
    | k == IO.Raylib.Key_Kp_Add = Core.Tactile.Key_Kp_Add
    | k == IO.Raylib.Key_Kp_Enter = Core.Tactile.Key_Kp_Enter
    | k == IO.Raylib.Key_Kp_Equal = Core.Tactile.Key_Kp_Equal
    | otherwise = Core.Tactile.Key_Null

findArt :: String -> M.Map String (Ptr a) -> Ptr a
findArt k m = fromJust (M.lookup k m)

toRectangle :: Position -> Dimensions -> (ScaleFactor, ScaleFactor) -> IO.Raylib.Rectangle
toRectangle p (w, h) (s1, s2) = IO.Raylib.Rectangle (realToFrac x') (realToFrac y') (realToFrac w') (realToFrac h')
                                     where (w', h') = scaleDimensions (w, h) (s1, s2)
                                           (x', y') = translatePosition p (s1, s2)

drawText :: Ptr Font -> String -> Position -> ScaleFactor -> IO ()
drawText f s p sf = do bs <- baseSize f
                       drawTextEx f s (IO.Raylib.Vector2 (fromIntegral (x p)) (fromIntegral (y p))) (fromIntegral bs * sf) sf white


translatePosition :: Position -> (ScaleFactor, ScaleFactor) -> (Float, Float)
translatePosition p (ws, hs) = (fromIntegral (x p) * ws, fromIntegral (y p) * hs)

scaleDimensions :: Dimensions -> (ScaleFactor, ScaleFactor) -> (Float, Float)
scaleDimensions (w, h) (ws, hs) = (fromIntegral w * ws, fromIntegral h * hs)