{
   $Id$
   System independent system interface for win32

   Copyright (c) 2000 by Pierre Muller

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
   windows,dos,winevent;

var
   ChangeSystemEvents : TCriticalSection;
Const
  SystemEventActive : Boolean = false;

procedure SystemEventHandler(var ir:INPUT_RECORD);

  var
     e : TSystemEvent;

  begin
     if (ir.EventType in [FOCUS_EVENT,WINDOW_BUFFER_SIZE_EVENT]) then
       begin
          EnterCriticalSection(ChangeSystemEvents);
          if (ir.EventType=FOCUS_EVENT) then
            begin
              if ir.Event.FocusEvent.bSetFocus then
                e.typ:=SysSetFocus
              else
                e.typ:=SysReleaseFocus;
            end
          else
            begin
              e.typ:=SysResize;
              e.x:=ir.Event.WindowBufferSizeEvent.dwSize.x;
              e.y:=ir.Event.WindowBufferSizeEvent.dwSize.y;
            end;
          PutSystemEvent(e);
          LeaveCriticalSection(ChangeSystemEvents);
       end;
  end;

procedure InitSystemMsg;

var
   mode : dword;

begin
  if SystemEventActive then
    exit;
  // enable Window events
  GetConsoleMode(TextRec(Input).Handle,@mode);
  mode:=mode or ENABLE_WINDOW_INPUT;
  SetConsoleMode(TextRec(Input).Handle,mode);

  PendingSystemHead:=@PendingSystemEvent;
  PendingSystemTail:=@PendingSystemEvent;
  PendingSystemEvents:=0;
  FillChar(LastSystemEvent,sizeof(TSystemEvent),0);
  InitializeCriticalSection(ChangeSystemEvents);
  SetResizeEventHandler(@SystemEventHandler);
  SetFocusEventHandler(@SystemEventHandler);
  SystemEventActive:=true;
end;


procedure DoneSystemMsg;
var
   mode : dword;
begin
  if not SystemEventActive then
    exit;
  // disable System events
  GetConsoleMode(TextRec(Input).Handle,@mode);
  mode:=mode and (not ENABLE_WINDOW_INPUT);
  SetConsoleMode(TextRec(Input).Handle,mode);

  SetResizeEventHandler(nil);
  SetFocusEventHandler(nil);
  DeleteCriticalSection(ChangeSystemEvents);
  SystemEventActive:=false;
end;



procedure GetSystemEvent(var SystemEvent: TSystemEvent);

var
   b : byte;

begin
  repeat
    EnterCriticalSection(ChangeSystemEvents);
    b:=PendingSystemEvents;
    LeaveCriticalSection(ChangeSystemEvents);
    if b>0 then
      break
    else
      sleep(50);
  until false;
  EnterCriticalSection(ChangeSystemEvents);
  SystemEvent:=PendingSystemHead^;
  inc(PendingSystemHead);
  if longint(PendingSystemHead)=longint(@PendingSystemEvent)+sizeof(PendingSystemEvent) then
   PendingSystemHead:=@PendingSystemEvent;
  dec(PendingSystemEvents);
  LastSystemEvent:=SystemEvent;
  LeaveCriticalSection(ChangeSystemEvents);
end;


function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
begin
  EnterCriticalSection(ChangeSystemEvents);
  if PendingSystemEvents>0 then
   begin
     SystemEvent:=PendingSystemHead^;
     PollSystemEvent:=true;
   end
  else
   PollSystemEvent:=false;
  LeaveCriticalSection(ChangeSystemEvents);
end;

{
  $Log$
  Revision 1.1  2002-05-21 11:59:57  pierre
   + system messages unit added

}
