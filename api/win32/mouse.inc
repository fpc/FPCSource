{
   $Id$
   System independent mouse interface for win32

   Copyright (c) 1999 by Florian Klaempfl

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
}

uses
   windows,dos,event;

var
   ChangeMouseEvents : TCriticalSection;
Const
  MouseEventActive : Boolean = false;

procedure MouseEventHandler;

  var
     ir : INPUT_RECORD;
     dwRead : DWord;
     i: longint;
     e : TMouseEvent;

  begin
     ReadConsoleInput(TextRec(Input).Handle,ir,1,dwRead);
     if (dwRead=1) and (ir.EventType=_MOUSE_EVENT) then
       begin
          EnterCriticalSection(ChangeMouseEvents);
          e.x:=ir.MouseEvent.dwMousePosition.x;
          e.y:=ir.MouseEvent.dwMousePosition.y;
          e.buttons:=0;
          e.action:=0;
          if (ir.MouseEvent.dwButtonState and FROM_LEFT_1ST_BUTTON_PRESSED<>0) then
            e.buttons:=e.buttons or MouseLeftButton;
          if (ir.MouseEvent.dwButtonState and FROM_LEFT_2ND_BUTTON_PRESSED<>0) then
            e.buttons:=e.buttons or MouseMiddleButton;
          if (ir.MouseEvent.dwButtonState and RIGHTMOST_BUTTON_PRESSED<>0) then
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
               // this should be done in PutMouseEvent
               inc(PendingMouseEvents);
            end;
          LeaveCriticalSection(ChangeMouseEvents);
       end;
  end;

procedure InitMouse;

var
   mode : dword;

begin
  if MouseEventActive then
    exit;
  // enable mouse events
  GetConsoleMode(TextRec(Input).Handle,@mode);
  mode:=mode or ENABLE_MOUSE_INPUT;
  SetConsoleMode(TextRec(Input).Handle,mode);

  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
  InitializeCriticalSection(ChangeMouseEvents);
  SetMouseEventHandler(@MouseEventHandler);
  ShowMouse;
  MouseEventActive:=true;
end;


procedure DoneMouse;
var
   mode : dword;
begin
  if not MouseEventActive then
    exit;
  HideMouse;
  // disable mouse events
  GetConsoleMode(TextRec(Input).Handle,@mode);
  mode:=mode and (not ENABLE_MOUSE_INPUT);
  SetConsoleMode(TextRec(Input).Handle,mode);

  SetMouseEventHandler(nil);
  DeleteCriticalSection(ChangeMouseEvents);
  MouseEventActive:=false;
end;


function DetectMouse:byte;
var
  num : dword;
begin
  GetNumberOfConsoleMouseButtons(@num);
  DetectMouse:=num;
end;


procedure ShowMouse;
begin
end;


procedure HideMouse;
begin
end;


function GetMouseX:word;
begin
  GetMouseX:=0;
end;


function GetMouseY:word;
begin
  GetMouseY:=0;
end;


function GetMouseButtons:word;
begin
  GetMouseButtons:=0;
end;


procedure SetMouseXY(x,y:word);
begin
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);

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


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;
begin
  EnterCriticalSection(ChangeMouseEvents);
  if PendingMouseEvents>0 then
   begin
     MouseEvent:=PendingMouseHead^;
     PollMouseEvent:=true;
   end
  else
   PollMouseEvent:=false;
  LeaveCriticalSection(ChangeMouseEvents);
end;

{
  $Log$
  Revision 1.2  2000-07-13 11:32:27  michael
  + removed logs
 
}
