{ this program need GPM !! }

{$ifdef linux}
uses
  Gpm;
{$endif def linux}

var
  Conn: TGPMConnect;
  Quit: Boolean;
  Event: TGPMEvent;

begin

  FillChar(Conn, SizeOf(Conn), 0);
  Conn.EventMask := GPM_MOVE+GPM_DRAG+GPM_DOWN+GPM_UP+GPM_SINGLE+GPM_DOUBLE;
  Conn.DefaultMask := 0;
  GPM_Open(Conn, 0);
  WriteLn('I have opened the mouse... trying to do something tricky...');
  Quit := False;
  while not Quit do begin
    GPM_GetEvent(Event);
    WriteLn('GetEvent returned... Event.EventType=', Event.EventType);
    if Event.EventType and GPM_BARE_EVENTS = GPM_DOWN then begin
      WriteLn('You have pressed a mouse button...');
      Quit := True;
    end;
  end;
  GPM_Close;
{$else def linux}
begin
{$endif def linux}
end.