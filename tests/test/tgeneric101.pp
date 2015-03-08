{ %FAIL }

program tgeneric101;

{$mode objfpc}

uses
  ugeneric99;

type
  TTest1 = specialize TTestClass.TTest<LongInt>;

begin

end.
