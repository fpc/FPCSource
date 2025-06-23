{$mode delphi}
procedure proc(arg: array of const);
var
  p: array[0..999] of TVarRec absolute arg;
begin
  writeln(p[0].VInteger);
  writeln(String(p[1].VPointer));
  writeln(String(p[2].VPointer));
end;

begin
  proc([100, 'foo', 'bar']);
end.
