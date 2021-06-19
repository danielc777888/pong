--TODO: Try using color defs from raylib, and not coding for it
--TODO: Remove unecessary function bindings, eg. drawTexture as drawTextureEx is being used
--TODO: More keyboardkey enums
--TODO: Add mouse support
--TODO: Variadic argument support for traceLog

module IO.Raylib (
 Texture2D,
 Font (Font),
 Sound,
 Vector2 (Vector2),
 Color,
 KeyboardKey (..),
 TraceLogType (..),
 Rectangle (Rectangle),
 --core window
 initWindow,
 windowShouldClose,
 closeWindow,
 toggleFullScreen,
 getMonitorWidth,
 getMonitorHeight,
  --core drawing
 beginDrawing,
 endDrawing,
 --core timing
 setTargetFPS,
 --core input
 isKeyPressed,
 isKeyDown,
 --core misc
 traceLog,
 --textures loading
 loadTexture,
 unloadTexture,
  --textures drawing
 drawTextureEx,
 drawTexturePro,
 --text loading
 loadFont,
 unloadFont,
 --text drawing
 drawTextEx,
 --text auxiliary
 baseSize,
 --audio device management
 initAudioDevice,
 --audio loading
 loadSound,
 unloadSound,
 --audio management
 playSound,
 --colors
 white) where

import Foreign.Storable
import Foreign
import Foreign.C

#include <raylib.h>

type FileName = String
type X = Int
type Y = Int
type Width = Int
type Height = Int
type Title = String
type FPS = Int
  
data Texture2D = Texture2D CInt CInt CInt CInt CInt
instance Storable Texture2D where
  sizeOf _ = #{size Texture2D}
  alignment _ = #{alignment Texture2D}
  peek ptr = do
    id' <- (#peek Texture2D, id) ptr
    width' <- (#peek Texture2D, width) ptr
    height' <- (#peek Texture2D, height) ptr
    mipmaps' <- (#peek Texture2D, mipmaps) ptr
    format' <- (#peek Texture2D, format) ptr
    return (Texture2D id' width' height' mipmaps' format')
  poke ptr (Texture2D id' width' height' mipmaps' format') = do
    (#poke Texture2D, id) ptr id'
    (#poke Texture2D, width) ptr width'
    (#poke Texture2D, height) ptr height'
    (#poke Texture2D, mipmaps) ptr mipmaps'
    (#poke Texture2D, format) ptr format'

data Color = Color CUChar CUChar CUChar CUChar
instance Storable Color where
  sizeOf _ = #{size Color}
  alignment _ = #{alignment Color}
  peek ptr = do
    r' <- (#peek Color, r) ptr
    g' <- (#peek Color, g) ptr
    b' <- (#peek Color, b) ptr
    a' <- (#peek Color, a) ptr
    return (Color r' g' b' a')
  poke ptr (Color r' g' b' a') = do
    (#poke Color, r) ptr r'
    (#poke Color, g) ptr g'
    (#poke Color, b) ptr b'
    (#poke Color, a) ptr a'

data KeyboardKey = Space
                 | Enter
                 | KRight
                 | KLeft
                 | Down
                 | Up
                 | LShift
                 | LControl
                 | LAlt
                 | RShift
                 | RControl
                 | RAlt
                 | Escape deriving (Show, Eq)
instance Enum KeyboardKey where
  fromEnum Space = #{const KEY_SPACE}
  fromEnum Enter = #{const KEY_ENTER}
  fromEnum KRight = #{const KEY_RIGHT}
  fromEnum KLeft = #{const KEY_LEFT}
  fromEnum Down = #{const KEY_DOWN}
  fromEnum Up = #{const KEY_UP}
  fromEnum LShift = #{const KEY_LEFT_SHIFT}
  fromEnum LControl = #{const KEY_LEFT_CONTROL}
  fromEnum LAlt = #{const KEY_LEFT_ALT}
  fromEnum RShift = #{const KEY_RIGHT_SHIFT}
  fromEnum RControl = #{const KEY_RIGHT_CONTROL}
  fromEnum RAlt = #{const KEY_RIGHT_ALT}
  fromEnum Escape = #{const KEY_ESCAPE}
  toEnum #{const KEY_SPACE} = Space
  toEnum #{const KEY_ENTER} = Enter
  toEnum #{const KEY_RIGHT} = KRight
  toEnum #{const KEY_LEFT} = KLeft
  toEnum #{const KEY_DOWN} = Down
  toEnum #{const KEY_UP} = Up
  toEnum #{const KEY_LEFT_SHIFT} = LShift
  toEnum #{const KEY_LEFT_CONTROL} = LControl
  toEnum #{const KEY_LEFT_ALT} = LAlt
  toEnum #{const KEY_RIGHT_SHIFT} = RShift
  toEnum #{const KEY_RIGHT_CONTROL} = RControl
  toEnum #{const KEY_RIGHT_ALT} = RAlt
  toEnum #{const KEY_ESCAPE} = Escape

data AudioStream = AudioStream (Ptr CInt) CUInt CUInt CUInt
instance Storable AudioStream where
  sizeOf _ = #{size AudioStream}
  alignment _ = #{alignment AudioStream}
  peek ptr = do
    buffer' <- (#peek AudioStream, buffer) ptr
    sampleRate' <- (#peek AudioStream, sampleRate) ptr
    sampleSize' <- (#peek AudioStream, sampleSize) ptr
    channels' <- (#peek AudioStream, channels) ptr
    return (AudioStream buffer' sampleRate' sampleSize' channels')
  poke ptr (AudioStream buffer' sampleRate' sampleSize' channels') = do
    (#poke AudioStream, buffer) ptr buffer'
    (#poke AudioStream, sampleRate) ptr sampleRate'
    (#poke AudioStream, sampleSize) ptr sampleSize'
    (#poke AudioStream, channels) ptr channels'

data Sound = Sound AudioStream CUInt
instance Storable Sound where
  sizeOf _ = #{size Sound}
  alignment _ = #{alignment Sound}
  peek ptr = do
    stream' <- (#peek Sound, stream) ptr
    sampleCount' <- (#peek Sound, sampleCount) ptr
    return (Sound stream' sampleCount')
  poke ptr (Sound stream' sampleCount') = do
    (#poke Sound, stream) ptr stream'
    (#poke Sound, sampleCount) ptr sampleCount'

data Vector2 = Vector2 CFloat CFloat
instance Storable Vector2 where
  sizeOf _ = #{size Vector2}
  alignment _ = #{alignment Vector2}
  peek ptr = do
    x' <- (#peek Vector2, x) ptr
    y' <- (#peek Vector2, y) ptr
    return (Vector2 x' y')
  poke ptr (Vector2 x' y') = do
    (#poke Vector2, x) ptr x'
    (#poke Vector2, y) ptr y'

data Image = Image (Ptr CInt) CInt CInt CInt CInt
instance Storable Image where
  sizeOf _ = #{size Image}
  alignment _ = #{alignment Image}
  peek ptr = do
    data' <- (#peek Image, data) ptr
    width' <- (#peek Image, width) ptr
    height' <- (#peek Image, height) ptr
    mipmaps' <- (#peek Image, mipmaps) ptr
    format' <- (#peek Image, format) ptr
    return (Image data' width' height' mipmaps' format')
  poke ptr (Image data' width' height' mipmaps' format') = do
    (#poke Image, data) ptr data'
    (#poke Image, width) ptr width'
    (#poke Image, height) ptr height'
    (#poke Image, mipmaps) ptr mipmaps'
    (#poke Image, format) ptr format'

data CharInfo = CharInfo CInt CInt CInt CInt Image
instance Storable CharInfo where
  sizeOf _ = #{size CharInfo}
  alignment _ = #{alignment CharInfo}
  peek ptr = do
    value' <- (#peek CharInfo, value) ptr
    offsetX' <- (#peek CharInfo, offsetX) ptr
    offsetY' <- (#peek CharInfo, offsetY) ptr
    advanceX' <- (#peek CharInfo, advanceX) ptr
    image' <- (#peek CharInfo, image) ptr
    return (CharInfo value' offsetX' offsetY' advanceX' image')
  poke ptr (CharInfo value' offsetX' offsetY' advanceX' image') = do
    (#poke CharInfo, value) ptr value'
    (#poke CharInfo, offsetX) ptr offsetX'
    (#poke CharInfo, offsetY) ptr offsetY'
    (#poke CharInfo, advanceX) ptr advanceX'
    (#poke CharInfo, image) ptr image'

data Rectangle = Rectangle CFloat CFloat CFloat CFloat
instance Storable Rectangle where
  sizeOf _ = #{size Rectangle}
  alignment _ = #{alignment Rectangle}
  peek ptr = do
    x' <- (#peek Rectangle, x) ptr
    y' <- (#peek Rectangle, y) ptr
    width' <- (#peek Rectangle, width) ptr
    height' <- (#peek Rectangle, height) ptr
    return (Rectangle x' y' width' height')
  poke ptr (Rectangle x' y' width' height') = do
    (#poke Rectangle, x) ptr x'
    (#poke Rectangle, y) ptr y'
    (#poke Rectangle, width) ptr width'
    (#poke Rectangle, height) ptr height'

data Font = Font CInt CInt CInt Texture2D (Ptr Rectangle) (Ptr CharInfo)
instance Storable Font where
  sizeOf _ = #{size Font}
  alignment _ = #{alignment Font}
  peek ptr = do
    baseSize' <- (#peek Font, baseSize) ptr
    charsCount' <- (#peek Font, charsCount) ptr
    charsPadding' <- (#peek Font, charsPadding) ptr
    texture' <- (#peek Font, texture) ptr
    recs' <- (#peek Font, recs) ptr
    chars' <- (#peek Font, chars) ptr
    return (Font baseSize' charsCount' charsPadding' texture' recs' chars')
  poke ptr (Font baseSize' charsCount' charsPadding' texture' recs' chars') = do
    (#poke Font, baseSize) ptr baseSize'
    (#poke Font, charsCount) ptr charsCount'
    (#poke Font, charsPadding) ptr charsPadding'
    (#poke Font, texture) ptr texture'
    (#poke Font, recs) ptr recs'
    (#poke Font, chars) ptr chars'


data TraceLogType = All
                  | Trace
                  | Debug
                  | Info
                  | Warning
                  | Error
                  | Fatal
                  | None deriving (Show, Eq)
instance Enum TraceLogType where
  fromEnum All = #{const LOG_ALL}
  fromEnum Trace = #{const LOG_TRACE}
  fromEnum Debug = #{const LOG_DEBUG}
  fromEnum Info = #{const LOG_INFO}
  fromEnum Warning = #{const LOG_WARNING}
  fromEnum Error = #{const LOG_ERROR}
  fromEnum Fatal = #{const LOG_FATAL}
  fromEnum None = #{const LOG_NONE}
  toEnum #{const LOG_ALL} = All
  toEnum #{const LOG_TRACE} = Trace
  toEnum #{const LOG_DEBUG} = Debug
  toEnum #{const LOG_INFO} = Info
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_ERROR} = Error
  toEnum #{const LOG_FATAL} = Fatal
  toEnum #{const LOG_NONE} = None


--colors
white :: Color
white = Color 255 255 255 255

blank :: Color
blank = Color 0 0 0 0

--core window
foreign import ccall unsafe "raylib.h InitWindow" c_initWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Width -> Height -> Title -> IO ()
initWindow w h t = do t' <- newCString t
                      c_initWindow (fromIntegral w) (fromIntegral h) t'

foreign import ccall unsafe "raylib.h WindowShouldClose" c_windowShouldClose :: IO CBool
windowShouldClose :: IO Bool
windowShouldClose = do
                    done <- c_windowShouldClose
                    return (toBool done)

foreign import ccall unsafe "raylib.h CloseWindow" c_closeWindow :: IO ()
closeWindow :: IO ()
closeWindow = do c_closeWindow

foreign import ccall unsafe "raylib.h ToggleFullscreen" c_toggleFullScreen :: IO ()
toggleFullScreen :: IO ()
toggleFullScreen = do c_toggleFullScreen

foreign import ccall unsafe "raylib.h GetMonitorWidth" c_getMonitorWidth :: CInt -> IO CInt
getMonitorWidth :: Int -> IO Int
getMonitorWidth x = do w <- c_getMonitorWidth (fromIntegral x)
                       return (fromIntegral w)

foreign import ccall unsafe "raylib.h GetMonitorHeight" c_getMonitorHeight :: CInt -> IO CInt
getMonitorHeight :: Int -> IO Int
getMonitorHeight x = do h <- c_getMonitorHeight (fromIntegral x)
                        return (fromIntegral h)

--core drawing
foreign import ccall unsafe "raylib.h BeginDrawing" c_beginDrawing :: IO ()
beginDrawing :: IO ()
beginDrawing = do c_beginDrawing

foreign import ccall unsafe "raylib.h EndDrawing" c_endDrawing :: IO ()
endDrawing :: IO ()
endDrawing = do c_endDrawing

--core timing
foreign import ccall unsafe "raylib.h SetTargetFPS" c_setTargetFPS :: CInt -> IO ()
setTargetFPS :: FPS -> IO ()
setTargetFPS fps = do c_setTargetFPS (fromIntegral fps)

--core input
foreign import ccall unsafe "raylib.h IsKeyPressed" c_isKeyPressed :: CInt -> IO CBool
isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed k = do p <- c_isKeyPressed (fromIntegral (fromEnum k))
                    return (toBool p)

foreign import ccall unsafe "raylib.h IsKeyDown" c_isKeyDown :: CInt -> IO CBool
isKeyDown :: KeyboardKey -> IO Bool
isKeyDown k = do p <- c_isKeyDown (fromIntegral (fromEnum k))
                 return (toBool p)
--core misc
foreign import ccall unsafe "raylib.h TraceLog" c_traceLog :: CInt -> CString -> IO ()
traceLog :: TraceLogType -> String -> IO ()
traceLog t s = do s' <- newCString s
                  c_traceLog (fromIntegral (fromEnum t)) s'

--texture loading
foreign import ccall unsafe "c_raylib.h C_LoadTexture" c_loadTexture :: CString -> IO (Ptr Texture2D)
loadTexture :: FileName -> IO (Ptr Texture2D)
loadTexture f = do f' <- newCString f
                   c_loadTexture f'

foreign import ccall unsafe "c_raylib.h C_UnloadTexture" c_unloadTexture :: Ptr Texture2D -> IO ()
unloadTexture :: Ptr Texture2D -> IO ()
unloadTexture ptr = do c_unloadTexture ptr

--texture drawing
foreign import ccall unsafe "c_raylib.h C_DrawTextureEx" c_drawTextureEx :: Ptr Texture2D -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
drawTextureEx :: Ptr Texture2D -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureEx t_ptr v r s c = with c (\c_ptr ->
                                        with v (\v_ptr -> c_drawTextureEx t_ptr v_ptr (realToFrac r) (realToFrac s) c_ptr))

foreign import ccall unsafe "c_raylib.h C_DrawTexturePro" c_drawTexturePro :: Ptr Texture2D -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawTexturePro :: Ptr Texture2D -> Rectangle -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTexturePro t_ptr rc rc' v r c = with c (\c_ptr ->
                                                with rc (\rc_ptr ->
                                                           with rc' (\rc_ptr' ->
                                                                       with v (\v_ptr -> c_drawTexturePro t_ptr rc_ptr rc_ptr' v_ptr (realToFrac r) c_ptr))))

--audio device management
foreign import ccall unsafe "raylib.h InitAudioDevice" c_initAudioDevice :: IO ()
initAudioDevice :: IO ()
initAudioDevice = do c_initAudioDevice

--audio loading
foreign import ccall unsafe "c_raylib.h C_LoadSound" c_loadSound :: CString -> IO (Ptr Sound)
loadSound :: FileName -> IO (Ptr Sound)
loadSound f = do f' <- newCString f
                 c_loadSound f'

foreign import ccall unsafe "c_raylib.h C_UnloadSound" c_unloadSound :: Ptr Sound -> IO ()
unloadSound :: Ptr Sound -> IO ()
unloadSound ptr = do c_unloadSound ptr

--audio management
foreign import ccall unsafe "c_raylib.h C_PlaySound" c_playSound :: Ptr Sound -> IO ()
playSound :: Ptr Sound -> IO ()
playSound s_ptr = do c_playSound s_ptr


--text loading
foreign import ccall unsafe "c_raylib.h C_LoadFont" c_loadFont :: CString -> IO (Ptr Font)
loadFont :: FileName -> IO (Ptr Font)
loadFont f = do f' <- newCString f
                c_loadFont f'

foreign import ccall unsafe "c_raylib.h C_UnloadFont" c_unloadFont :: Ptr Font -> IO ()
unloadFont :: Ptr Font -> IO ()
unloadFont ptr = do c_unloadFont ptr

--text drawing
foreign import ccall unsafe "c_raylib.h C_DrawTextEx" c_drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> Float -> Float -> Ptr Color -> IO ()
drawTextEx :: Ptr Font -> String -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx f_ptr t v fs s c = with c (\c_ptr ->
                                   with v (\v_ptr ->
                                        withCString t (\t' -> c_drawTextEx f_ptr t' v_ptr (realToFrac fs) (realToFrac s) c_ptr)))

--text auxiliary
baseSize :: Ptr Font -> IO Int
baseSize ptr = do Font bs _ _ _ _ _ <- peek ptr
                  return (fromIntegral bs)