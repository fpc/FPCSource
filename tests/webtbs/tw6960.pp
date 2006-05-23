function SomeFunc(var Res: array of Double): Integer;
begin
  // do something
  writeln(High(Res));
end;

var
  d: Double;
  darr: array[1..3] of Double;
begin
  SomeFunc(d); // Delphi accepts this, FPC does not
  SomeFunc(darr); // OK
end.

