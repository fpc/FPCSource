{$MODE OBJFPC}
program variant_bug;
uses variants;

var SomeArray : array[1..10] of DWord;
    v         : Variant;
    y: longint;
begin
  for y := 1 to 10 do SomeArray[y] := 0;
  v := 7;
  SomeArray[ v ] := 1;
  for y := 1 to 10 do Write( SomeArray[y] );
  writeln;
  if somearray[v]<>1 then
    halt(1);
end.

