{ %target=i386 }
{ Source provided for Free Pascal Bug Report 2377 }
{ Submitted by "Christian Keck" on  2003-02-12 }
{ e-mail: c.keck@gmx.net }
program testkey;

uses keyboard;

var Key : TkeyEvent;
    i,j : longint;
begin
  { InitKeyBoard; }
  i:=0;
  j:=0;
  repeat
  begin
    if PollKeyEvent <> 0 then
    begin
      Key:= GetKeyEvent;
      Key:= TranslateKeyEvent(Key);
      writeln (ord(GetKeyEventChar(Key)));
      inc(j);
    end;
    inc(i);
  end;
  until i=50;
  if j<>0 then
    begin
      Writeln('Keyboard unit generates events without being started');
      halt(1);
    end;
  DoneKeyBoard;
end.
