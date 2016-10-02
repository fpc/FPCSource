{ %target=linux,darwin,freebsd,openbsd,netbsd,aix,solaris,android,win32,win64 }

{$mode objfpc}

uses
  sysutils;

procedure foo;
var
  s: PChar = 'PChar';
  b: boolean;
begin
  b:=false;
  try
    s[0] := 'a';
  except
    on e: exception do
      begin
        if e is EAccessViolation then
          b:=true;
      end;
  end;
  if not b then
    halt(1);
end;

begin
  foo;
end.
