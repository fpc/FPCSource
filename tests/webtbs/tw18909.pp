{$mode objfpc}
uses uw18909a, uw18909b;
var
  a: TA = (x: 1);
  b: TB = (x: 1);
begin
  b := a;
  Write(b.x);
end.
