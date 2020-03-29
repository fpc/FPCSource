{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Nikolay Nikolov
    member of the Free Pascal development team

    Keyboard unit for Win16

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Keyboard;
interface

{$i keybrdh.inc}

implementation

uses
  WinProcs, WinTypes, video;

{$i keyboard.inc}

const
  VK_A = $41;
  VK_B = $42;
  VK_C = $43;
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_G = $47;
  VK_H = $48;
  VK_I = $49;
  VK_J = $4A;
  VK_K = $4B;
  VK_L = $4C;
  VK_M = $4D;
  VK_N = $4E;
  VK_O = $4F;
  VK_P = $50;
  VK_Q = $51;
  VK_R = $52;
  VK_S = $53;
  VK_T = $54;
  VK_U = $55;
  VK_V = $56;
  VK_W = $57;
  VK_X = $58;
  VK_Y = $59;
  VK_Z = $5A;

var
  KbdBuf: array [0..15] of TKeyEvent;
  KbdBufHead, KbdBufTail: SmallInt;
  KbdShiftState: Byte;
  KbdState: TKeyboardState;


function KbdBufEmpty: Boolean; inline;
begin
  KbdBufEmpty:=KbdBufHead=KbdBufTail;
end;


procedure KbdBufEnqueue(k: TKeyEvent);
var
  nk: SmallInt;
begin
  nk:=(KbdBufHead+1) and 15;
  if nk<>KbdBufTail then
  begin
    KbdBuf[KbdBufHead]:=k;
    KbdBufHead:=nk;
  end;
end;


function KbdBufDequeue: TKeyEvent;
begin
  KbdBufDequeue:=KbdBuf[KbdBufTail];
  KbdBufTail:=(KbdBufTail+1) and 15;
end;


function KeyWndProc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
  procedure SimBiosKey(Code: Word);
  begin
    KbdBufEnqueue((kbPhys shl 24) or Code or (KbdShiftState shl 16));
  end;
var
  k: TKeyEvent;
  charbuf: array [0..3] of Byte;
  charcount, i: SmallInt;
begin
  case msg of
    WM_KEYDOWN,
    WM_SYSKEYDOWN:
      begin
        case wParam of
          VK_SHIFT:
            if Byte(lParam shr 16)=$36 then
              KbdShiftState:=KbdShiftState or %0001
            else
              KbdShiftState:=KbdShiftState or %0010;
          VK_CONTROL:
            KbdShiftState:=KbdShiftState or %0100;
          VK_MENU:
            KbdShiftState:=KbdShiftState or %1000;
        end;
        GetKeyboardState(FarAddr(KbdState));
        charcount:=ToAscii(wParam,Byte(lParam shr 16),FarAddr(KbdState),FarAddr(charbuf),0);
        if charcount>0 then
          for i:=0 to charcount-1 do
            KbdBufEnqueue((kbPhys shl 24) or charbuf[i] or (KbdShiftState shl 16));
        { TODO: implement all keys and shift/alt/ctrl combinations }
        { Alt? }
        if (KbdShiftState and %1000) <> 0 then
          case wParam of
            VK_ESCAPE:
              SimBiosKey($0100);
            VK_RETURN:
              SimBiosKey($1C00); { or $A600 for the numpad enter }
            VK_F1:
              SimBiosKey($6800);
            VK_F2:
              SimBiosKey($6900);
            VK_F3:
              SimBiosKey($6A00);
            VK_F4:
              SimBiosKey($6B00);
            VK_F5:
              SimBiosKey($6C00);
            VK_F6:
              SimBiosKey($6D00);
            VK_F7:
              SimBiosKey($6E00);
            VK_F8:
              SimBiosKey($6F00);
            VK_F9:
              SimBiosKey($7000);
            VK_F10:
              SimBiosKey($7100);
            VK_F11:
              SimBiosKey($8B00);
            VK_F12:
              SimBiosKey($8C00);
            VK_A:
              SimBiosKey($1E00);
            VK_B:
              SimBiosKey($3000);
            VK_C:
              SimBiosKey($2E00);
            VK_D:
              SimBiosKey($2000);
            VK_E:
              SimBiosKey($1200);
            VK_F:
              SimBiosKey($2100);
            VK_G:
              SimBiosKey($2200);
            VK_H:
              SimBiosKey($2300);
            VK_I:
              SimBiosKey($1700);
            VK_J:
              SimBiosKey($2400);
            VK_K:
              SimBiosKey($2500);
            VK_L:
              SimBiosKey($2600);
            VK_M:
              SimBiosKey($3200);
            VK_N:
              SimBiosKey($3100);
            VK_O:
              SimBiosKey($1800);
            VK_P:
              SimBiosKey($1900);
            VK_Q:
              SimBiosKey($1000);
            VK_R:
              SimBiosKey($1300);
            VK_S:
              SimBiosKey($1F00);
            VK_T:
              SimBiosKey($1400);
            VK_U:
              SimBiosKey($1600);
            VK_V:
              SimBiosKey($2F00);
            VK_W:
              SimBiosKey($1100);
            VK_X:
              SimBiosKey($2D00);
            VK_Y:
              SimBiosKey($1500);
            VK_Z:
              SimBiosKey($2C00);
          end
        { Ctrl? }
        else if (KbdShiftState and %0100) <> 0 then
          case wParam of
            VK_ESCAPE:
              SimBiosKey($011B);
            VK_RETURN:
              SimBiosKey($1C0A); { or $E00A for the numpad enter }
            VK_F1:
              SimBiosKey($5E00);
            VK_F2:
              SimBiosKey($5F00);
            VK_F3:
              SimBiosKey($6000);
            VK_F4:
              SimBiosKey($6100);
            VK_F5:
              SimBiosKey($6200);
            VK_F6:
              SimBiosKey($6300);
            VK_F7:
              SimBiosKey($6400);
            VK_F8:
              SimBiosKey($6500);
            VK_F9:
              SimBiosKey($6600);
            VK_F10:
              SimBiosKey($6700);
            VK_F11:
              SimBiosKey($8900);
            VK_F12:
              SimBiosKey($8A00);
          end
        { Shift? }
        else if (KbdShiftState and %0011) <> 0 then
          case wParam of
            VK_ESCAPE:
              SimBiosKey($011B);
            VK_RETURN:
              SimBiosKey($1C0D); { or $E00D for the numpad enter }
            VK_F1:
              SimBiosKey($5400);
            VK_F2:
              SimBiosKey($5500);
            VK_F3:
              SimBiosKey($5600);
            VK_F4:
              SimBiosKey($5700);
            VK_F5:
              SimBiosKey($5800);
            VK_F6:
              SimBiosKey($5900);
            VK_F7:
              SimBiosKey($5A00);
            VK_F8:
              SimBiosKey($5B00);
            VK_F9:
              SimBiosKey($5C00);
            VK_F10:
              SimBiosKey($5D00);
            VK_F11:
              SimBiosKey($8700);
            VK_F12:
              SimBiosKey($8800);
          end
        else
          case wParam of
            VK_ESCAPE:
              SimBiosKey($011B);
            VK_RETURN:
              SimBiosKey($1C0D); { or $E00D for the numpad enter }
            VK_F1:
              SimBiosKey($3B00);
            VK_F2:
              SimBiosKey($3C00);
            VK_F3:
              SimBiosKey($3D00);
            VK_F4:
              SimBiosKey($3E00);
            VK_F5:
              SimBiosKey($3F00);
            VK_F6:
              SimBiosKey($4000);
            VK_F7:
              SimBiosKey($4100);
            VK_F8:
              SimBiosKey($4200);
            VK_F9:
              SimBiosKey($4300);
            VK_F10:
              SimBiosKey($4400);
            VK_F11:
              SimBiosKey($8500);
            VK_F12:
              SimBiosKey($8600);
            VK_LEFT:
              SimBiosKey($4B00);
            VK_UP:
              SimBiosKey($4800);
            VK_DOWN:
              SimBiosKey($5000);
            VK_RIGHT:
              SimBiosKey($4D00);
            VK_PRIOR: { Page Up }
              SimBiosKey($4900);
            VK_NEXT:  { Page Down }
              SimBiosKey($5100);
            VK_HOME:
              SimBiosKey($4700);
            VK_END:
              SimBiosKey($4F00);
            VK_INSERT:
              SimBiosKey($5200);
            VK_DELETE:
              SimBiosKey($5300);
          end;
      end;
    WM_KEYUP,
    WM_SYSKEYUP:
      begin
        case wParam of
          VK_SHIFT:
            if Byte(lParam shr 16)=$36 then
              KbdShiftState:=KbdShiftState and %11111110
            else
              KbdShiftState:=KbdShiftState and %11111101;
          VK_CONTROL:
            KbdShiftState:=KbdShiftState and %11111011;
          VK_MENU:
            KbdShiftState:=KbdShiftState and %11110111;
        end;
      end;
  end;
  KeyWndProc:=DefWindowProc(hwnd,msg,wParam,lParam);
end;


procedure SysInitKeyboard;
begin
  video.KeyEventWndProc:=@KeyWndProc;
  KbdBufHead:=0;
  KbdBufTail:=0;
end;


function SysGetKeyEvent: TKeyEvent;
var
  m: MSG;
begin
  while KbdBufEmpty and GetMessage(FarAddr(m),0,0,0) do
  begin
    TranslateMessage(FarAddr(m));
    DispatchMessage(FarAddr(m));
  end;
  if KbdBufEmpty then
    SysGetKeyEvent:=0
  else
    SysGetKeyEvent:=KbdBufDequeue;
end;


function SysPollKeyEvent: TKeyEvent;
var
  m: MSG;
begin
  while PeekMessage(FarAddr(m),0,0,0,1) do
  begin
    TranslateMessage(FarAddr(m));
    DispatchMessage(FarAddr(m));
  end;
  if KbdBufEmpty then
    SysPollKeyEvent:=0
  else
    SysPollKeyEvent:=KbdBuf[KbdBufTail];
end;


function SysGetShiftState: Byte;
begin
  SysGetShiftState:=KbdShiftState;
end;


Const
  SysKeyboardDriver : TKeyboardDriver = (
    InitDriver : @SysInitKeyboard;
    DoneDriver : Nil;
    GetKeyevent : @SysGetKeyEvent;
    PollKeyEvent : @SysPollKeyEvent;
    GetShiftState : @SysGetShiftState;
    TranslateKeyEvent : Nil;
    TranslateKeyEventUnicode : Nil;
  );

begin
  SetKeyBoardDriver(SysKeyBoardDriver);
end.
