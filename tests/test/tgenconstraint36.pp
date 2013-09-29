{ %FAIL }

program tgenconstraint36;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest21TTestObject1 = TTest21<TTestObject1>;

begin

end.
