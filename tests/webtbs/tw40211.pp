{ %opt=-Ct -Cs7340032 }
{ %target=win32,win64,linux,freebsd,darwin,openbsd }
program project1;

{$mode objfpc}

function BigStack: Integer;
var
  buf: array[0..6*1024*1014] of Byte;
begin
  buf[0] := 1;
  buf[High(buf)] := 2;
  Result := buf[0] + buf[High(buf)];
end;

begin
  if BigStack <> 3 then
    Halt(1);
end.
