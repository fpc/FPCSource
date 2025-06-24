{$mode delphi}
program ie99080501;

procedure proc(arg: array of const);
var
  p: TVarRec absolute arg;
begin
  writeln(PtrInt(@p)); // Error: Internal error 99080501
end;

begin
  proc([100, 'foo', 'bar']);
end.
