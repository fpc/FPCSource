{$mode macpas}
program test;
var r: record field: packed array [0..1] of byte; b:  byte; end;
procedure p(a: pointer); begin end;
begin p(@r.field[0]);
end.

