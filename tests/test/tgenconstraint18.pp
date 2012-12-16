{ %FAIL }

program tgenconstraint18;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest7TTestClass5 = TTest7<TTestClass5>;

begin

end.
