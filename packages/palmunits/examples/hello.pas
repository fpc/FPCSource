{$APPID FPHL}
{$APPNAME Hello, FPC}
program hello;

uses
  event_, sysevent, systemmgr, window;

const
  message = 'FPC says: Hello, Palm!';

procedure EventLoop;
var
  event: EventType;
begin
  repeat
    EvtGetEvent(event, evtWaitForever);
    SysHandleEvent(event);
  until (event.eType = appStopEvent);
end;

begin
  WinDrawChars(message, length(message), 35, 74);
  EventLoop;
end.
