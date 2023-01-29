{ %opt=-O4 -CE -Oonoconstprop }
{$mode objfpc}
uses
  sysutils;
var
  a : real;
begin
  try
    writeln(a/a);
  except
    halt(1);
  end;
  halt(0);
end.
