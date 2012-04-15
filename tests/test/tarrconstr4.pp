{ %fail }

program tarrconstr4;

{$mode delphi}

type
  TAB = array[0..1] of byte;
var
  D: TAB;
begin
  // only dynamic arrays are allowed
  D := TAB.Create(1, 1);
end.

