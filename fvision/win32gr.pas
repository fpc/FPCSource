{
    $Id$
    Copyright (c) 2002 by Pierre Muller

    This unit implements an the hooks needed for the win32 graph unit.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit win32gr;

interface

procedure SetGraphHooks;

procedure UnsetGraphHooks;

implementation

uses
  windows,
  drivers,
  graph;


var
  InputHandle : Handle;
  StoredControlKeyState : longint;
  lastir : INPUT_RECORD;

const
  LastChar : char = #0;


const
  KeyToAsciiCode : array [0..255] of char =
  (
  { 00 } #0,
  { 01 VK_LBUTTON } #0,
  { 02 VK_RBUTTON } #0,
  { 03 VK_CANCEL } #0,
  { 04 VK_MBUTTON } #0,
  { 05 unassigned } #0,
  { 06 unassigned } #0,
  { 07 unassigned } #0,
  { 08 VK_BACK } #8,
  { 09 VK_TAB } #9,
  { 0A unassigned } #0,
  { 0B unassigned } #0,
  { 0C VK_CLEAR ?? } #0,
  { 0D VK_RETURN } #13,
  { 0E unassigned } #0,
  { 0F unassigned } #0,
  { 10 VK_SHIFT } #0,
  { 11 VK_CONTROL } #0,
  { 12 VK_MENU (Alt key) } #0,
  { 13 VK_PAUSE } #0,
  { 14 VK_CAPITAL (Caps Lock) } #0,
  { 15 Reserved for Kanji systems} #0,
  { 16 Reserved for Kanji systems} #0,
  { 17 Reserved for Kanji systems} #0,
  { 18 Reserved for Kanji systems} #0,
  { 19 Reserved for Kanji systems} #0,
  { 1A unassigned } #0,
  { 1B VK_ESCAPE } #27,
  { 1C Reserved for Kanji systems} #0,
  { 1D Reserved for Kanji systems} #0,
  { 1E Reserved for Kanji systems} #0,
  { 1F Reserved for Kanji systems} #0,
  { 20 VK_SPACE} ' ',
  { 21 VK_PRIOR (PgUp) } #0,
  { 22 VK_NEXT (PgDown) } #0,
  { 23 VK_END } #0,
  { 24 VK_HOME } #0,
  { 25 VK_LEFT } #0,
  { 26 VK_UP } #0,
  { 27 VK_RIGHT } #0,
  { 28 VK_DOWN } #0,
  { 29 VK_SELECT ??? } #0,
  { 2A OEM specific !! } #0,
  { 2B VK_EXECUTE } #0,
  { 2C VK_SNAPSHOT } #0,
  { 2D VK_INSERT } #0,
  { 2E VK_DELETE } #0,
  { 2F VK_HELP } #0,
  { 30 VK_0 '0' } '0',
  { 31 VK_1 '1' } '1',
  { 32 VK_2 '2' } '2',
  { 33 VK_3 '3' } '3',
  { 34 VK_4 '4' } '4',
  { 35 VK_5 '5' } '5',
  { 36 VK_6 '6' } '6',
  { 37 VK_7 '7' } '7',
  { 38 VK_8 '8' } '8',
  { 39 VK_9 '9' } '9',
  { 3A unassigned } #0,
  { 3B unassigned } #0,
  { 3C unassigned } #0,
  { 3D unassigned } #0,
  { 3E unassigned } #0,
  { 3F unassigned } #0,
  { 40 unassigned } #0,
  { 41 VK_A 'A' } 'A',
  { 42 VK_B 'B' } 'B',
  { 43 VK_C 'C' } 'C',
  { 44 VK_D 'D' } 'D',
  { 45 VK_E 'E' } 'E',
  { 46 VK_F 'F' } 'F',
  { 47 VK_G 'G' } 'G',
  { 48 VK_H 'H' } 'H',
  { 49 VK_I 'I' } 'I',
  { 4A VK_J 'J' } 'J',
  { 4B VK_K 'K' } 'K',
  { 4C VK_L 'L' } 'L',
  { 4D VK_M 'M' } 'M',
  { 4E VK_N 'N' } 'N',
  { 4F VK_O 'O' } 'O',
  { 50 VK_P 'P' } 'P',
  { 51 VK_Q 'Q' } 'Q',
  { 52 VK_R 'R' } 'R',
  { 53 VK_S 'S' } 'S',
  { 54 VK_T 'T' } 'T',
  { 55 VK_U 'U' } 'U',
  { 56 VK_V 'V' } 'V',
  { 57 VK_W 'W' } 'W',
  { 58 VK_X 'X' } 'X',
  { 59 VK_Y 'Y' } 'Y',
  { 5A VK_Z 'Z' } 'Z',
  { 5B unassigned } #0,
  { 5C unassigned } #0,
  { 5D unassigned } #0,
  { 5E unassigned } #0,
  { 5F unassigned } #0,
  { 60 VK_NUMPAD0 NumKeyPad '0' } '0',
  { 61 VK_NUMPAD1 NumKeyPad '1' } '1',
  { 62 VK_NUMPAD2 NumKeyPad '2' } '2',
  { 63 VK_NUMPAD3 NumKeyPad '3' } '3',
  { 64 VK_NUMPAD4 NumKeyPad '4' } '4',
  { 65 VK_NUMPAD5 NumKeyPad '5' } '5',
  { 66 VK_NUMPAD6 NumKeyPad '6' } '6',
  { 67 VK_NUMPAD7 NumKeyPad '7' } '7',
  { 68 VK_NUMPAD8 NumKeyPad '8' } '8',
  { 69 VK_NUMPAD9 NumKeyPad '9' } '9',
  { 6A VK_MULTIPLY } #0,
  { 6B VK_ADD } #0,
  { 6C VK_SEPARATOR } #0,
  { 6D VK_SUBSTRACT } #0,
  { 6E VK_DECIMAL } #0,
  { 6F VK_DIVIDE } #0,
  { 70 VK_F1 'F1' } #0,
  { 71 VK_F2 'F2' } #0,
  { 72 VK_F3 'F3' } #0,
  { 73 VK_F4 'F4' } #0,
  { 74 VK_F5 'F5' } #0,
  { 75 VK_F6 'F6' } #0,
  { 76 VK_F7 'F7' } #0,
  { 77 VK_F8 'F8' } #0,
  { 78 VK_F9 'F9' } #0,
  { 79 VK_F10 'F10' } #0,
  { 7A VK_F11 'F11' } #0,
  { 7B VK_F12 'F12' } #0,
  { 7C VK_F13 } #0,
  { 7D VK_F14 } #0,
  { 7E VK_F15 } #0,
  { 7F VK_F16 } #0,
  { 80 VK_F17 } #0,
  { 81 VK_F18 } #0,
  { 82 VK_F19 } #0,
  { 83 VK_F20 } #0,
  { 84 VK_F21 } #0,
  { 85 VK_F22 } #0,
  { 86 VK_F23 } #0,
  { 87 VK_F24 } #0,
  { 88 unassigned } #0,
  { 89 VK_NUMLOCK } #0,
  { 8A VK_SCROLL } #0,
  { 8B unassigned } #0,
  { 8C unassigned } #0,
  { 8D unassigned } #0,
  { 8E unassigned } #0,
  { 8F unassigned } #0,
  { 90 unassigned } #0,
  { 91 unassigned } #0,
  { 92 unassigned } #0,
  { 93 unassigned } #0,
  { 94 unassigned } #0,
  { 95 unassigned } #0,
  { 96 unassigned } #0,
  { 97 unassigned } #0,
  { 98 unassigned } #0,
  { 99 unassigned } #0,
  { 9A unassigned } #0,
  { 9B unassigned } #0,
  { 9C unassigned } #0,
  { 9D unassigned } #0,
  { 9E unassigned } #0,
  { 9F unassigned } #0,
  { A0 unassigned } #0,
  { A1 unassigned } #0,
  { A2 unassigned } #0,
  { A3 unassigned } #0,
  { A4 unassigned } #0,
  { A5 unassigned } #0,
  { A6 unassigned } #0,
  { A7 unassigned } #0,
  { A8 unassigned } #0,
  { A9 unassigned } #0,
  { AA unassigned } #0,
  { AB unassigned } #0,
  { AC unassigned } #0,
  { AD unassigned } #0,
  { AE unassigned } #0,
  { AF unassigned } #0,
  { B0 unassigned } #0,
  { B1 unassigned } #0,
  { B2 unassigned } #0,
  { B3 unassigned } #0,
  { B4 unassigned } #0,
  { B5 unassigned } #0,
  { B6 unassigned } #0,
  { B7 unassigned } #0,
  { B8 unassigned } #0,
  { B9 unassigned } #0,
  { BA OEM specific } #0,
  { BB OEM specific } #0,
  { BC OEM specific } #0,
  { BD OEM specific } #0,
  { BE OEM specific } #0,
  { BF OEM specific } #0,
  { C0 OEM specific } #0,
  { C1 unassigned } #0,
  { C2 unassigned } #0,
  { C3 unassigned } #0,
  { C4 unassigned } #0,
  { C5 unassigned } #0,
  { C6 unassigned } #0,
  { C7 unassigned } #0,
  { C8 unassigned } #0,
  { C9 unassigned } #0,
  { CA unassigned } #0,
  { CB unassigned } #0,
  { CC unassigned } #0,
  { CD unassigned } #0,
  { CE unassigned } #0,
  { CF unassigned } #0,
  { D0 unassigned } #0,
  { D1 unassigned } #0,
  { D2 unassigned } #0,
  { D3 unassigned } #0,
  { D4 unassigned } #0,
  { D5 unassigned } #0,
  { D6 unassigned } #0,
  { D7 unassigned } #0,
  { D8 unassigned } #0,
  { D9 unassigned } #0,
  { DA unassigned } #0,
  { DB OEM specific } #0,
  { DC OEM specific } #0,
  { DD OEM specific } #0,
  { DE OEM specific } #0,
  { DF OEM specific } #0,
  { E0 OEM specific } #0,
  { E1 OEM specific } #0,
  { E2 OEM specific } #0,
  { E3 OEM specific } #0,
  { E4 OEM specific } #0,
  { E5 unassigned } #0,
  { E6 OEM specific } #0,
  { E7 unassigned } #0,
  { E8 unassigned } #0,
  { E9 OEM specific } #0,
  { EA OEM specific } #0,
  { EB OEM specific } #0,
  { EC OEM specific } #0,
  { ED OEM specific } #0,
  { EE OEM specific } #0,
  { EF OEM specific } #0,
  { F0 OEM specific } #0,
  { F1 OEM specific } #0,
  { F2 OEM specific } #0,
  { F3 OEM specific } #0,
  { F4 OEM specific } #0,
  { F5 OEM specific } #0,
  { F6 unassigned } #0,
  { F7 unassigned } #0,
  { F8 unassigned } #0,
  { F9 unassigned } #0,
  { FA unassigned } #0,
  { FB unassigned } #0,
  { FC unassigned } #0,
  { FD unassigned } #0,
  { FE unassigned } #0,
  { FF unassigned } #0
  );

{ this procedure allows to hook keyboard messages }
function fvisioncharmessagehandler (Window: hwnd; AMessage, WParam,
                                  LParam: Longint): Longint;
var
  ir : INPUT_RECORD;
  NumWritten : longint;
  vKey : byte;
  IsExtended : boolean;
begin
  fvisioncharmessagehandler:=0;
  if (AMessage = WM_CHAR) then
    begin
      if LastChar<>#0 then
        begin
          Writeln('char ',chr(wparam and $ff),' $',hexstr(wparam,2));
          Lastir.Event.KeyEvent.AsciiChar:=chr(wparam and $ff);
          WriteConsoleInput(InputHandle,lastir,1,NumWritten);
          LastChar:=#0;
        end
      else
        begin
          Writeln('char ',chr(wparam and $ff),' $',hexstr(wparam,2),' ignored');
        end;
      exit;
    end;
  if LastChar<>#0 then
    begin
      WriteConsoleInput(InputHandle,lastir,1,NumWritten);
      LastChar:=#0;
    end;
  fillchar(ir,sizeof(ir),#0);
  ir.EventType:=KEY_EVENT;
  with ir.Event.KeyEvent do
    begin
      vKey:=WParam and $ff;
      wRepeatCount:=lparam and $ffff;
      IsExtended:=(lParam and (1 shl 24))<>0;
      if AMessage = WM_KEYDOWN then
        bKeyDown:=true;
      wVirtualKeyCode:=vKey;
      AsciiChar:=KeyToAsciiCode[vKey];
      if AsciiChar<>#0 then
        begin
          { Use lower chars }
          if ((StoredControlKeyState and SHIFT_PRESSED)=0) and
            ((wVirtualKeyCode>=VK_A) and (wVirtualKeyCode<=VK_Z)) then
              AsciiChar:=chr(ord(AsciiChar) + ord('a')-ord('A'));
          if bKeyDown then
            LastChar:=AsciiChar;
        end;
      case vKey of
        VK_SHIFT :
          if bKeyDown then
            StoredControlKeyState:= StoredControlKeyState or SHIFT_PRESSED
          else
            StoredControlKeyState:= StoredControlKeyState and not SHIFT_PRESSED;
        VK_CONTROL :
          begin
            if IsExtended then
              begin
                if bKeyDown then
                  StoredControlKeyState:= StoredControlKeyState or RIGHT_CTRL_PRESSED
                else
                  StoredControlKeyState:= StoredControlKeyState and not RIGHT_CTRL_PRESSED;
              end
            else if bKeyDown then
              StoredControlKeyState:= StoredControlKeyState or LEFT_CTRL_PRESSED
            else
              StoredControlKeyState:= StoredControlKeyState and not LEFT_CTRL_PRESSED;
          end;
        VK_MENU :
          begin
            if IsExtended then
              begin
                if bKeyDown then
                  StoredControlKeyState:= StoredControlKeyState or RIGHT_ALT_PRESSED
                else
                  StoredControlKeyState:= StoredControlKeyState and not RIGHT_ALT_PRESSED;
              end
            else if bKeyDown then
              StoredControlKeyState:= StoredControlKeyState or LEFT_ALT_PRESSED
            else
              StoredControlKeyState:= StoredControlKeyState and not LEFT_ALT_PRESSED;
          end;
        end;
      dwControlKeyState:=StoredControlKeyState;
    end;
  if Lastchar=#0 then
    WriteConsoleInput(InputHandle,ir,1,NumWritten)
  else
    Lastir:=ir;
end;

{ this procedure allows to hook mouse messages }
function fvisionmousemessagehandler (Window: hwnd; AMessage, WParam,
                                   LParam: Longint): Longint;

var
  ir : INPUT_RECORD;
  NumWritten : longint;
begin
  fvisionmousemessagehandler:=0;
  ir.EventType:=_MOUSE_EVENT;
  with ir.Event.MouseEvent do
    begin
      dwMousePosition.x:=loword(LParam) div SysFontWidth;
      dwMousePosition.y:=hiword(LParam) div SysFontHeight;
      dwButtonState:=0;
      if (wParam and MK_LBUTTON)<>0 then
        dwButtonState:=dwButtonState or FROM_LEFT_1ST_BUTTON_PRESSED;
      if (wParam and MK_MBUTTON)<>0 then
        dwButtonState:=dwButtonState or FROM_LEFT_2ND_BUTTON_PRESSED;
      if (wParam and MK_RBUTTON)<>0 then
        dwButtonState:=dwButtonState or RIGHTMOST_BUTTON_PRESSED;
    end;
  WriteConsoleInput(InputHandle,ir,1,NumWritten);
end;

procedure SetGraphHooks;

begin
  mousemessagehandler:=@fvisionmousemessagehandler;
  charmessagehandler:=@fvisioncharmessagehandler;
  InputHandle:=GetStdHandle(STD_INPUT_HANDLE);
end;

procedure UnsetGraphHooks;

begin
  mousemessagehandler:=nil;
  charmessagehandler:=nil;
  InputHandle:=UnusedHandle;
  StoredControlKeyState:=0;
end;

end.

{
  $Log$
  Revision 1.2  2002-05-28 19:12:26  pierre
   * fix fvisioncharmessage

  Revision 1.1  2002/05/24 09:35:20  pierre
   first commit, not fully functional yet


}
