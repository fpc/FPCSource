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
  if not(SameValue(s1,0)) or not(SameValue(s2,1)) or not(SameValue(d1,0)) or
    not(SameValue(d2,1)) or not(SameValue(e1,0)) or not(SameValue(e2,1)) then
    halt(1);

  sincos(pi,s1,s2);
  sincos(pi,d1,d2);
  sincos(pi,e1,e2);
  if not(SameValue(s1,0)) or not(SameValue(s2,-1)) or not(SameValue(d1,0)) or
    not(SameValue(d2,-1)) or not(SameValue(e1,0)) or not(SameValue(e2,-1)) then
    halt(1);
  writeln('ok');
end.

