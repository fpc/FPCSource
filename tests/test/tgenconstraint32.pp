{ %FAIL }

program tgenconstraint32;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest17ITestTTestClass = TTest17<ITest1, TTestClass>;

begin

end.
