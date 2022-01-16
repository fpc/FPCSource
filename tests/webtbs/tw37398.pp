program testnan;

{$mode objfpc}{$H+}

uses math;
var d: double;
  temp: Boolean;
begin
  d := NaN;
  temp := IsNan(d) or (d = 0);
  writeln(temp);
end.
