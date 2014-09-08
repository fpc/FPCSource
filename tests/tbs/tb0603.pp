{ %OPT=-O3 }
{ %norun }
program test4;
var
  S : ansistring;
begin
  writeln((PByte(@S[1])^ = $1) or (S[1] = '.'));
end.
