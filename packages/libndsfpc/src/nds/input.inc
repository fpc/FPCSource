{$ifdef NDS_INTERFACE}
// Keyboard
type
  KEYPAD_BITS = cint;
const
//!	Bit values for the keypad buttons.
  KEY_A      : KEYPAD_BITS = (1 shl 0);  // Keypad A button.
  KEY_B      : KEYPAD_BITS = (1 shl 1);  // Keypad B button.
  KEY_SELECT : KEYPAD_BITS = (1 shl 2);  // Keypad SELECT button.
  KEY_START  : KEYPAD_BITS = (1 shl 3);  // Keypad START button.
  KEY_RIGHT  : KEYPAD_BITS = (1 shl 4);  // Keypad RIGHT button.
  KEY_LEFT   : KEYPAD_BITS = (1 shl 5);  // Keypad LEFT button.
  KEY_UP     : KEYPAD_BITS = (1 shl 6);  // Keypad UP button.
  KEY_DOWN   : KEYPAD_BITS = (1 shl 7);  // Keypad DOWN button.
  KEY_R      : KEYPAD_BITS = (1 shl 8);  // Right shoulder button.
  KEY_L      : KEYPAD_BITS = (1 shl 9);  // Left shoulder button.
  KEY_X      : KEYPAD_BITS = (1 shl 10); // Keypad X button.
  KEY_Y      : KEYPAD_BITS = (1 shl 11); // Keypad Y button.
  KEY_TOUCH  : KEYPAD_BITS = (1 shl 12); // Touchscreen pendown.
  KEY_LID    : KEYPAD_BITS = (1 shl 13); // Lid state.

  REG_KEYINPUT : pcuint16 = pointer($04000130);
  REG_KEYCNT   : pcuint16 = pointer($04000132);

{$endif NDS_INTERFACE}
