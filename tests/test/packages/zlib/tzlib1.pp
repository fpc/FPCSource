{ %NOTE=This test requires an installed zlib1 shared library }
{ simple compilation test }
uses
  zlib;
begin
  writeln(zlibVersion);
end.
