{$mode objfpc}
{$ModeSwitch StatementExpressions}

type
  tbase = object i: integer; end;
  tchild1 = object(tbase) end;
  tchild2 = object(tbase) end;

function c1: tchild1;
begin
  result.i:=42;
end;


function c2: tchild2;
begin
  result.i:=32;
end;

var
  b:tbase;
begin
  b := if 0 < 1 then c1 else c2;
  WriteLn(b.i);
  if (b.i<>42) then
    Halt(1);
end.
