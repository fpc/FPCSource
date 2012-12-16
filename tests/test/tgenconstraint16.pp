{ %FAIL }

program tgenconstraint16;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest5TTestObject1 = TTest5<TTestObject1>;

begin

end.
