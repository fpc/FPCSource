{$mode macpas}

program test;
var x: packed array [1..8] of boolean;
begin x[1]:= false; x[1]:= not x[1];
if (not x[1]) then
  halt(1);
end.

