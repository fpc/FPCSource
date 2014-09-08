uses
  math;
var
  s1,s2 : single;
  d1,d2 : double;
  e1,e2 : extended;

begin
  sincos(0,s1,s2);
  sincos(0,d1,d2);
  sincos(0,e1,e2);
  if not(SameValue(s1,0)) then
    halt(1);
  if not(SameValue(s2,1)) then
    halt(2);
  if not(SameValue(d1,0)) then
    halt(3);
  if not(SameValue(d2,1)) then
    halt(4);
  if not(SameValue(e1,0)) then
    halt(5);
  if not(SameValue(e2,1)) then
    halt(6);

  sincos(pi,s1,s2);
  sincos(pi,d1,d2);
  sincos(pi,e1,e2);
  if not(SameValue(s1,0)) then
    halt(7);
  if not(SameValue(s2,-1)) then
    halt(8);
  if not(SameValue(d1,0)) then
    halt(9);
  if not(SameValue(d2,-1)) then
    halt(10);
  if not(SameValue(e1,0)) then
    halt(11);
  if not(SameValue(e2,-1)) then
    halt(12);
  writeln('ok');
end.

