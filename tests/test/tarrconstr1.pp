program tarrconstr1;

{$mode delphi}

type
  TAB = array of byte;
var
  D: TAB;
  i: integer;
begin
  i:=1;
  D := TAB.create(1+2, i);
  if Length(D) <> 2 then
    halt(1);
  if D[0] <> 3 then
    halt(2);
  if D[1] <> i then
    halt(3);
end.

