{ %OPT=-O3 -OoDEADSTORE }
program tw41682;

{$mode objfpc}

function f(x: double): double; inline;
var a: double;
begin
  a := x;
  result := a;
end;

procedure k(var y: double); inline;
var z: double;
begin
  z := 1.0;
  y := f(z);
end;

var ty: double;
begin
  k(ty);
  if ty <> 1.0 then halt(1);
end.
