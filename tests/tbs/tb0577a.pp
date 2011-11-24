{$mode delphi}

const
  cdefaulthandler = 1;
  cinheritedhandler = 2;
  cunsupportedhandler = 3;

type
  tc = class
    procedure defaulthandler(var message); override;
    procedure handler(var message:longint); message cinheritedhandler;
  end;

  tc2 = class(tc)
    procedure handler(var message: longint);
  end;

  tc3 = class(tc2)
    procedure someproc(var message:tc3); message cinheritedhandler;
    procedure handler(var message:tc3); message cunsupportedhandler;
  end;

var
  glob: longint;

procedure tc.defaulthandler(var message);
begin
  glob:=cdefaulthandler;
end;

procedure tc.handler(var message: longint);
begin
  glob:=cinheritedhandler;
end;


procedure tc2.handler(var message: longint);
begin
  halt(1);
end;

procedure tc3.someproc(var message: tc3);
begin
  inherited;
end;

procedure tc3.handler(var message: tc3);
begin
  glob:=cunsupportedhandler;
  inherited
end;

var
  c: tc3;
begin
  c:=tc3.create;
  c.someproc(c);
  if glob<>cinheritedhandler then
    halt(2);
  c.handler(c);
  if glob<>cdefaulthandler then
    halt(3);
end.
