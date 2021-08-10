module Core.Tactile
  ( Tactile (Tactile),
    KeyboardKey (..),
    keysPressed,
    keysDown,
    touchedKey,
  )
where

import Data.Set (Set, member)

type KeysPressed = Set KeyboardKey

type KeysDown = Set KeyboardKey

data Tactile = Tactile KeysPressed KeysDown

data KeyboardKey
  = Key_Null
  | --alphanumeric
    Key_Apostrophe
  | Key_Comma
  | Key_Minus
  | Key_Period
  | Key_Slash
  | Key_Zero
  | Key_One
  | Key_Two
  | Key_Three
  | Key_Four
  | Key_Five
  | Key_Six
  | Key_Seven
  | Key_Eight
  | Key_Nine
  | Key_Semicolon
  | Key_Equal
  | Key_A
  | Key_B
  | Key_C
  | Key_D
  | Key_E
  | Key_F
  | Key_G
  | Key_H
  | Key_I
  | Key_J
  | Key_K
  | Key_L
  | Key_M
  | Key_N
  | Key_O
  | Key_P
  | Key_Q
  | Key_R
  | Key_S
  | Key_T
  | Key_U
  | Key_V
  | Key_W
  | Key_X
  | Key_Y
  | Key_Z
  | --functions
    Key_Space
  | Key_Escape
  | Key_Enter
  | Key_Tab
  | Key_Backspace
  | Key_Insert
  | Key_Delete
  | Key_Right
  | Key_Left
  | Key_Down
  | Key_Up
  | Key_Page_Up
  | Key_Page_Down
  | Key_Home
  | Key_End
  | Key_Caps_Lock
  | Key_Scroll_Lock
  | Key_Num_Lock
  | Key_Print_Screen
  | Key_Pause
  | Key_F1
  | Key_F2
  | Key_F3
  | Key_F4
  | Key_F5
  | Key_F6
  | Key_F7
  | Key_F8
  | Key_F9
  | Key_F10
  | Key_F11
  | Key_F12
  | Key_Left_Shift
  | Key_Left_Control
  | Key_Left_Alt
  | Key_Left_Super
  | Key_Right_Shift
  | Key_Right_Control
  | Key_Right_Alt
  | Key_Right_Super
  | Key_Kb_Menu
  | Key_Left_Bracket
  | Key_Backslash
  | Key_Right_Bracket
  | Key_Grave
  | --keypad
    Key_Kp_0
  | Key_Kp_1
  | Key_Kp_2
  | Key_Kp_3
  | Key_Kp_4
  | Key_Kp_5
  | Key_Kp_6
  | Key_Kp_7
  | Key_Kp_8
  | Key_Kp_9
  | Key_Kp_Decimal
  | Key_Kp_Divide
  | Key_Kp_Multiply
  | Key_Kp_Subtract
  | Key_Kp_Add
  | Key_Kp_Enter
  | Key_Kp_Equal
  deriving (Show, Eq, Ord, Enum)

touchedKey :: KeyboardKey -> Tactile -> Bool
touchedKey k t = member k (keysPressed t) || member k (keysDown t)

keysPressed :: Tactile -> KeysPressed
keysPressed (Tactile kp _) = kp

keysDown :: Tactile -> KeysDown
keysDown (Tactile _ kd) = kd