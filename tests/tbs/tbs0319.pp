{$ifdef fpc}{$mode delphi}{$endif}

function a:longint;
var
  a : longint;
begin
  a:=1;
end;

type
  cl=class
    k : longint;
    procedure p1;
    procedure p2;
  end;

procedure cl.p1;
var
  k : longint;
begin
end;

procedure cl.p2;
var
  p1 : longint;
begin
end;

begin
end.
