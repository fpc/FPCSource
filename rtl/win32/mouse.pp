{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Mouse unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Mouse;
interface

{$i mouseh.inc}

implementation

uses
   windows,dos,Winevent;

{$i mouse.inc}

var
   ChangeMouseEvents : TCriticalSection;

procedure MouseEventHandler(var ir:INPUT_RECORD);

  var
     e : TMouseEvent;

  begin
          EnterCriticalSection(ChangeMouseEvents);
          e.x:=ir.Event.MouseEvent.dwMousePosition.x;
          e.y:=ir.Event.MouseEvent.dwMousePosition.y;
          e.buttons:=0;
          e.action:=0;
          if (ir.Event.MouseEvent.dwButtonState and FROM_LEFT_1ST_BUTTON_PRESSED<>0) then
            e.buttons:=e.buttons or MouseLeftButton;
          if (ir.Event.MouseEvent.dwButtonState and FROM_LEFT_2ND_BUTTON_PRESSED<>0) then
            e.buttons:=e.buttons or MouseMiddleButton;
          if (ir.Event.MouseEvent.dwButtonState and RIGHTMOST_BUTTON_PRESSED<>0) then
            e.buttons:=e.buttons or MouseRightButton;

          { can we compress the events? }
          if (PendingMouseEvents>0) and
            (e.buttons=PendingMouseTail^.buttons) and
            (e.action=PendingMouseTail^.action) then
            begin
               PendingMouseTail^.x:=e.x;
               PendingMouseTail^.y:=e.y;
            end
          else
            begin
               PutMouseEvent(e);
               // this should be done in PutMouseEvent, now it is PM
               // inc(PendingMouseEvents);
            end;
          LeaveCriticalSection(ChangeMouseEvents);
  end;

procedure SysInitMouse;

var
   mode : dword;

begin
  // enable mouse events
  GetConsoleMode(StdInputHandle,@mode);
  mode:=mode or ENABLE_MOUSE_INPUT;
  SetConsoleMode(StdInputHandle,mode);

  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
  InitializeCriticalSection(ChangeMouseEvents);
  SetMouseEventHandler(@MouseEventHandler);
  ShowMouse;
end;


procedure SysDoneMouse;
var
   mode : dword;
begin
  HideMouse;
  // disable mouse events
  GetConsoleMode(StdInputHandle,@mode);
  mode:=mode and (not ENABLE_MOUSE_INPUT);
  SetConsoleMode(StdInputHandle,mode);

  SetMouseEventHandler(nil);
  DeleteCriticalSection(ChangeMouseEvents);
end;


function SysDetectMouse:byte;
var
  num : dword;
begin
  GetNumberOfConsoleMouseButtons(@num);
  SysDetectMouse:=num;
end;


procedure SysGetMouseEvent(var MouseEvent: TMouseEvent);

var
   b : byte;

begin
  repeat
    EnterCriticalSection(ChangeMouseEvents);
    b:=PendingMouseEvents;
    LeaveCriticalSection(ChangeMouseEvents);
    if b>0 then
      break
    else
      sleep(50);
  until false;
  EnterCriticalSection(ChangeMouseEvents);
  MouseEvent:=PendingMouseHead^;
  inc(PendingMouseHead);
  if longint(PendingMouseHead)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
   PendingMouseHead:=@PendingMouseEvent;
  dec(PendingMouseEvents);
  if (LastMouseEvent.x<>MouseEvent.x) or (LastMouseEvent.y<>MouseEvent.y) then
   MouseEvent.Action:=MouseActionMove;
  if (LastMouseEvent.Buttons<>MouseEvent.Buttons) then
   begin
     if (LastMouseEvent.Buttons=0) then
      MouseEvent.Action:=MouseActionDown
     else
      MouseEvent.Action:=MouseActionUp;
   end;
  LastMouseEvent:=MouseEvent;
  LeaveCriticalSection(ChangeMouseEvents);
end;


function SysPollMouseEvent(var MouseEvent: TMouseEvent):boolean;
begin
  EnterCriticalSection(ChangeMouseEvents);
  if PendingMouseEvents>0 then
   begin
     MouseEvent:=PendingMouseHead^;
     SysPollMouseEvent:=true;
   end
  else
   SysPollMouseEvent:=false;
  LeaveCriticalSection(ChangeMouseEvents);
end;


procedure SysPutMouseEvent(const MouseEvent: TMouseEvent);
begin
  if PendingMouseEvents<MouseEventBufSize then
   begin
     PendingMouseTail^:=MouseEvent;
     inc(PendingMouseTail);
     if longint(PendingMouseTail)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
      PendingMouseTail:=@PendingMouseEvent;
      { why isn't this done here ?
        so the win32 version do this by hand:}
       inc(PendingMouseEvents);
   end;
end;

Const
  SysMouseDriver : TMouseDriver = (
    UseDefaultQueue : False;
    InitDriver      : @SysInitMouse;
    DoneDriver      : @SysDoneMouse;
    DetectMouse     : @SysDetectMouse;
    ShowMouse       : Nil;
    HideMouse       : Nil;
    GetMouseX       : Nil;
    GetMouseY       : Nil;
    GetMouseButtons : Nil;
    SetMouseXY      : Nil;
    GetMouseEvent   : @SysGetMouseEvent;
    PollMouseEvent  : @SysPollMouseEvent;
    PutMouseEvent   : @SysPutMouseEvent;
  );

Begin
  SetMouseDriver(SysMouseDriver);
end.
{
  $Log$
  Revision 1.6  2002-09-07 16:01:29  peter
    * old logs removed and tabs fixed

}
