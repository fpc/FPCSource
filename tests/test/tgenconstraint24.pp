{ %FAIL }

program tgenconstraint24;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest12TTestClass3 = TTest12<TTestClass3>;

begin

end.
