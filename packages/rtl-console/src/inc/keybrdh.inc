{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  { We have an errorcode base of 1010 }
  errKbdBase                    = 1010;
  errKbdInitError               = errKbdBase + 0;
  errKbdNotImplemented          = errKbdBase + 1;

type
  TKeyEvent = Cardinal;
  TKeyRecord = packed record
{$IFDEF ENDIAN_LITTLE}
    KeyCode : Word;
    ShiftState, Flags : Byte;
{$ELSE}
    Flags, ShiftState : Byte;
    KeyCode : Word;
{$ENDIF}
  end;

{ The structure of a TKeyEvent follows in LSB-MSB order:
  2 bytes: depending on flags either the physical representation of a key
           (under DOS scancode, ascii code pair), or the translated
           ASCII/unicode character
  1 byte:  shift-state when this key was pressed (or shortly after)
  1 byte:  flags, the following flags are defined:
           bit0-1
                   0: the lowest two bytes is the translated ASCII value
                   1: the lowest two bytes is the translated Unicode value
                      (wide-char)
                   2: the lowest two bytes is a function key, and the lowest
                      two bytes contains its platform independent code
                   3: the lowest two bytes is the physical representation
           bit2
                   0: the key is pressed
                   1: the key is released (This event is not guaranteed to occur on all platforms)
           bit3-7  undefined, should be 0


  If there are two keys returning the same char-code, there's no way to find
  out which one was pressed (Gray+ and Simple+). If you need to know which
  was pressed, you'll need to use the untranslated keycodes, which is system
  dependent. System dependent constants may be defined to cover those, with
  possibily having the same name (but different value). }

{ System independent function key codes }
const
  kbdF1        = $FF01;
  kbdF2        = $FF02;
  kbdF3        = $FF03;
  kbdF4        = $FF04;
  kbdF5        = $FF05;
  kbdF6        = $FF06;
  kbdF7        = $FF07;
  kbdF8        = $FF08;
  kbdF9        = $FF09;
  kbdF10       = $FF0A;
  kbdF11       = $FF0B;
  kbdF12       = $FF0C;
  kbdF13       = $FF0D;
  kbdF14       = $FF0E;
  kbdF15       = $FF0F;
  kbdF16       = $FF10;
  kbdF17       = $FF11;
  kbdF18       = $FF12;
  kbdF19       = $FF13;
  kbdF20       = $FF14;

  kbdLWin      = $FF15;
  kbdRWin      = $FF16;
  kbdApps      = $FF17;

  { $15 - $1F reserved for future Fxx keys }
  kbdHome      = $FF20;
  kbdUp        = $FF21;
  kbdPgUp      = $FF22;
  kbdLeft      = $FF23;
  kbdMiddle    = $FF24;
  kbdRight     = $FF25;
  kbdEnd       = $FF26;
  kbdDown      = $FF27;
  kbdPgDn      = $FF28;

  kbdInsert    = $FF29;
  kbdDelete    = $FF2A;
  { $2B - $2F reserved for future keypad keys }

  { possible flag values }
  kbASCII       = $00;
  kbUniCode     = $01;
  kbFnKey       = $02;
  kbPhys        = $03;

  kbReleased    = $04;

  { shiftstate flags }
  kbLeftShift   = 1;
  kbRightShift  = 2;
  kbShift       = kbLeftShift or kbRightShift;
  kbCtrl        = 4;
  kbAlt         = 8;

{ ---------------------------------------------------------------------
    Key names. Can be localized if needed.
  ---------------------------------------------------------------------}
  SShift       : Array [1..3] of string[5] = ('SHIFT','CTRL','ALT');
  SLeftRight   : Array [1..2] of string[5] = ('LEFT','RIGHT');
  SUnicodeChar : String = 'Unicode character ';
  SScanCode    : String = 'Key with scancode ';
  SUnknownFunctionKey : String = 'Unknown function key : ';
  SAnd         : String = 'AND';
  SKeyPad      : Array [0..($FF2F-kbdHome)] of string[6] =
                 ('Home','Up','PgUp','Left',
                  'Middle','Right','End','Down',
                  'PgDn','Insert','Delete','',
                  '','','','');

Type
  TKeyboardDriver = Record
    InitDriver : Procedure;
    DoneDriver : Procedure;
    GetKeyEvent : Function : TKeyEvent;
    PollKeyEvent : Function : TKeyEvent;
    GetShiftState : Function : Byte;
    TranslateKeyEvent : Function (KeyEvent: TKeyEvent): TKeyEvent;
    TranslateKeyEventUniCode : Function (KeyEvent: TKeyEvent): TKeyEvent;
  end;

procedure InitKeyboard;
{ Initializes the keyboard interface, additional platform specific parameters
  can be passed by global variables (RawMode etc.) for the first implementation
  under DOS it does nothing }

procedure DoneKeyboard;
{ Deinitializes the keyboard interface }

function GetKeyEvent: TKeyEvent;
{ Returns the last keyevent, and waits for one if not available }

procedure PutKeyEvent(KeyEvent: TKeyEvent);
{ Adds the given KeyEvent to the input queue. Please note that depending on
  the implementation this can hold only one value (NO FIFOs etc) }

function PollKeyEvent: TKeyEvent;
{ Checks if a keyevent is available, and returns it if one is found. If no
  event is pending, it returns 0 }

function PollShiftStateEvent: TKeyEvent;
{ Return the current shiftstate in a keyevent }

function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
{ Performs ASCII translation of the KeyEvent }

function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;
{ Performs Unicode translation of the KeyEvent }

function GetKeyEventFlags(KeyEvent: TKeyEvent): Byte;
{ Returns the flags part of the given KeyEvent }

function GetKeyEventChar(KeyEvent: TKeyEvent): Char;
{ Returns the charcode part of the given KeyEvent, if it contains a translated
  keycode }

function GetKeyEventUniCode(KeyEvent: TKeyEvent): Word;
{ Returns the unicode part of the given KeyEvent, if it contains a translated
  unicode character }

function GetKeyEventCode(KeyEvent: TKeyEvent): Word;
{ Returns the translated function keycode part of the given KeyEvent, if it
  contains a translated function keycode }

function GetKeyEventShiftState(KeyEvent: TKeyEvent): Byte;
{ Returns the shift-state values of the given KeyEvent }

function IsFunctionKey(KeyEvent: TKeyEvent): Boolean;
{ Returns true if the given key was a function key or not }

Function SetKeyboardDriver (Const Driver : TKeyboardDriver) : Boolean;
{ Sets the keyboard driver to use }

Procedure GetKeyboardDriver (Var Driver : TKeyboardDriver);
{ Returns the currently active keyboard driver }

Function ShiftStateToString(KeyEvent : TKeyEvent; UseLeftRight : Boolean) : String;
{ Returns a string representation of a shift state as returned by
  pollshiftstate }
Function FunctionKeyName (KeyCode : Word) : String;
{ Returns the name of a function key if the key is one of the special keys . }
Function KeyEventToString(KeyEvent : TKeyEvent) : String;
{ Returns a string representation of the pressed key }

