{
   $Id$

   Keyboard unit, part of the portable API for Pascal

   Copyright (c) 1997 Balazs Scheidler (bazsi@balabit.hu)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
unit Keyboard;

interface
{$i platform.inc}

uses
{$ifdef DEBUG}
{$ifdef win32}
  windows,
{$endif win32}
{$endif DEBUG}

  ApiComm;

const
  { We have an errorcode base of 1010 }
  errKbdInitError               = errKbdBase + 0;
  errKbdNotImplemented          = errKbdBase + 1;

type
  TKeyEvent = Longint;

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

var
  PendingKeyEvent : TKeyEvent;


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
{$ifdef DEBUG}
{$ifdef win32}
var last_ir : INPUT_RECORD;
{$endif win32}
{$endif DEBUG}

implementation

{ Include platform dependent routines }

{$i keyboard.inc}


{ Platform independent routines }

procedure PutKeyEvent(KeyEvent: TKeyEvent);
begin
  PendingKeyEvent := KeyEvent;
end;

function GetKeyEventFlags(KeyEvent: TKeyEvent): Byte;
begin
  GetKeyEventFlags := (KeyEvent and $FF000000) shr 24;
end;

function GetKeyEventChar(KeyEvent: TKeyEvent): Char;
begin
  if KeyEvent and $03000000 = $00000000 then
    GetKeyEventChar := Chr(KeyEvent and $000000FF)
   else
    GetKeyEventChar := #0;
end;

function GetKeyEventUniCode(KeyEvent: TKeyEvent): Word;
begin
  if KeyEvent and $03000000 = $01000000 then
    GetKeyEventUniCode := KeyEvent and $0000FFFF
   else
    GetKeyEventUniCode := 0;
end;

function GetKeyEventCode(KeyEvent: TKeyEvent): Word;
begin
  GetKeyEventCode := KeyEvent and $0000FFFF
end;

function GetKeyEventShiftState(KeyEvent: TKeyEvent): Byte;
begin
  GetKeyEventShiftState := (KeyEvent and $00FF0000) shr 16;
end;

function IsFunctionKey(KeyEvent: TKeyEvent): Boolean;
begin
  IsFunctionKey := KeyEvent and $03000000 = $02000000;
end;

end.
{
  $Log$
  Revision 1.2  2000-02-29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.2  1999/12/09 21:29:15  pierre
   + some debug code for win32

  Revision 1.3  1999/11/24 23:36:56  peter
    * moved to packages dir

  Revision 1.2  1998/12/12 19:12:58  peter
    * keyboard updates
    * make test target, make all only makes units

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.6  1998/10/29 12:49:47  peter
    * more fixes

  Revision 1.5  1998/10/28 21:18:22  peter
    * more fixes

  Revision 1.4  1998/10/26 11:22:51  peter
    * updates


   Date       Version   Who     Comments
   07/28/98   0.2       Bazsi   Added some constants
}