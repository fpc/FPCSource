{ %NORUN }
program tgenconst29;

{$mode objfpc}

type
  TRange = 3..4;

  generic TTest<const U: TRange> = record end;

var
  t: specialize TTest<3>;
begin
end.
