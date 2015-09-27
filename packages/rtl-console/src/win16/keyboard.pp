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
        GetKeyboardState(@KbdState);
        charcount:=ToAscii(wParam,Byte(lParam shr 16),@KbdState,@charbuf,0);
        if charcount>0 then
          for i:=0 to charcount-1 do
            KbdBufEnqueue((kbPhys shl 24) or charbuf[i] or (KbdShiftState shl 16));
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
  while KbdBufEmpty and GetMessage(@m,0,0,0) do
  begin
    TranslateMessage(@m);
    DispatchMessage(@m);
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
  while PeekMessage(@m,0,0,0,1) do
  begin
    TranslateMessage(@m);
    DispatchMessage(@m);
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
