{
    Copyright (c) 2018 Karoly Balogh

    Simple "Hello, World!" alike program for PalmOS
    Example program for Free Pascal's PalmOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPID FPHL}
{$APPNAME Hello, FPC}
program hello;

uses
  event_, sysevent, systemmgr, window, font;

const
  message = 'FPC says: Hello, Palm!';

procedure PaintMessage;
var
  w, h: smallint;
  tw, th: smallint;
begin
  tw:=FntCharsWidth(message, length(message));
  th:=FntLineHeight;
  WinGetWindowExtent(w, h);

  WinDrawChars(message, length(message), (w-tw) div 2, (h-th) div 2);
end;

procedure EventLoop;
var
  event: EventType;
begin
  repeat
    PaintMessage;
    EvtGetEvent(event, evtWaitForever);
    SysHandleEvent(event);
  until (event.eType = appStopEvent);
end;

begin
  EventLoop;
end.
