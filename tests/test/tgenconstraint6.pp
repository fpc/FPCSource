{ %FAIL }

program tgenconstraint6;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest1TTestObject1 = TTest1<TTestObject1>;

begin

end.
