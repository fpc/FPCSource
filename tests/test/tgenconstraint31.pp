{ %FAIL }

program tgenconstraint31;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest17TTestClassTTestClass = TTest17<TTestClass, TTestClass>;

begin

end.
