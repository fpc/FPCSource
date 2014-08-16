program tprop5a;

uses
  tprop5;
var
  d: tderivedclass;
begin
  d:=tderivedclass.create;
  d.level:=5;
  halt(d.level-5);
end.
