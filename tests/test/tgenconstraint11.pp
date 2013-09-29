{ %FAIL }

program tgenconstraint11;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest4TTestRec = TTest4<TTestRec>;

begin

end.
