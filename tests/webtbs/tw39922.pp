{ %OPT=-O3 }

{ Test triggers "mov $0,%reg" being inserted on the wrong side of a SETcc instruction }
program tw39922.pp;

{$mode objfpc}

uses sysutils;

function isAlpha(c : Byte): Integer;
begin
  if ((AnsiChar(c) >= 'A') and (AnsiChar(c) <= 'Z')) or
     ((AnsiChar(c) >= 'a') and (AnsiChar(c) <= 'z'))
  then
    Result := 1
  else
    Result := 0;
end;

begin
  if (isAlpha(Byte('u')) = 0) then
    Halt(1);

  if (isAlpha(Byte('A')) = 0) then
    Halt(2);

  if (isAlpha(Byte('2')) = 1) then
    Halt(3);
	
  WriteLn('ok');
end.
