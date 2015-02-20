{ %FAIL }

program tgeneric100;

{$mode objfpc}

uses
  ugeneric99;

type
  TTest1 = specialize ugeneric99.TTest<LongInt>;

begin

end.
