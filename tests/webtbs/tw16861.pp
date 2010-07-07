(*$packset 1 *)
program test;
var
  s8: set of 0..7;
  b: byte;
begin
  b:=17;
  s8:=[];
  if b in (s8+[1]) then
    halt(1);
  b:=5;
  if not(b in (s8+[5])) then
    halt(2);
end.
