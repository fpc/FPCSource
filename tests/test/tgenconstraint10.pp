{ %FAIL }

program tgenconstraint10;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest3TTestObject1 = TTest3<TTestObject1>;

begin

end.
