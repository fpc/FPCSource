{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  tc = class(tthread)
    procedure execute; override;
  end;

var
  caught: boolean;

procedure tc.execute;
type
  plongint = ^longint;
var
  p: plongint;
begin
  p:=nil;
  try
    writeln(p^);
  except
    caught:=true;
  end;
end;

var
  c: tc;
begin
  caught:=false;
  c:=tc.create(false);
  c.waitfor;
  c.free;
  halt(ord(not caught));
end.

