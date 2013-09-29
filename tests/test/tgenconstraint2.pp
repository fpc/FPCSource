{ %FAIL }

program tgenconstraint2;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest1TTestRec = TTest1<TTestRec>;

begin

end.
