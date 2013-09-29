{ %FAIL }

program tgenconstraint13;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest4TTestObject1 = TTest4<TTestObject1>;

begin

end.
