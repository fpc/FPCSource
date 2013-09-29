{ %FAIL }

program tgenconstraint34;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest21TTestRec = TTest21<TTestRec>;

begin

end.
