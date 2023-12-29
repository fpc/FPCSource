{ %OPT=-O3 }

{$mode objfpc}

program tval1;

function TryStrToInt(const s: string; out i: Longint): boolean; inline;
var
  Error : word;
begin
  Val(s, i, Error);
  TryStrToInt:=(Error=0)
end;

procedure DoTest;
var
  Output: Longint;
begin
  if TryStrToInt('Invalid', Output) then
    Halt(1);

  if not TryStrToInt('2', Output) then
    Halt(2);
	
  if Output <> 2 then
    Halt(3);
end;

begin
  DoTest(); { This is so "Output" is a local variable rather than global }
  WriteLn('ok');
end.