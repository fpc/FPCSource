{ tests accesses to implementation-section variables and typed constants
  (hidden symbols) of a unit, both from the unit itself and through inlined
  routines from another module }
uses
  ub0732;

var
  i: longint;
  expected: int64;

begin
  setvalues(3);
  expected:=3+3*$123456789+3+4+1+3+4+7+4;
  for i:=0 to 15 do
    inc(expected,i*3);
  if sumvalues<>expected then
    halt(1);
  if getaddrsl^<>3 then
    halt(2);
  getaddrsl^:=7;
  if getsl_inline<>7 then
    halt(3);
  for i:=0 to 7 do
    begin
      if gettab(i)<>i+1 then
        halt(4);
      if gettab_inline(i)<>i+1 then
        halt(5);
    end;
  writeln('ok');
end.
