{$mode objfpc}

{$Q+}
uses
  Sysutils;

begin
  writeln(Int64.MinValue+Int64(Random(0)));
end.
