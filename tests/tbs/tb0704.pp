{ %opt=-Oonofastmath -CE -Oonoconstprop }
{$mode objfpc}
uses
  sysutils;
var
  a : real;
begin
  try
    writeln(a/a);
  except
    halt(0);
  end;
  halt(1);
end.
