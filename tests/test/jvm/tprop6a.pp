program tprop6a;

uses
  tprop6;
var
  c: tbaseclassprop6;
  d: tderivedclassprop6;
begin
  c:=tbaseclassprop6.create;
  c.level:=4;
  if c.level<>4 then
    halt(1);
  d:=tderivedclassprop6.create;
  d.level:=5;
  halt(d.level-6);
end.
