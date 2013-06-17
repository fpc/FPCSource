{ %FAIL }

program tgenconstraint22;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest12TTestClass = TTest12<TTestClass>;

begin

end.
