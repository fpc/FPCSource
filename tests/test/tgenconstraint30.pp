{ %FAIL }

program tgenconstraint30;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest16TTestRec = TTest16<TTestRec>;

begin

end.
