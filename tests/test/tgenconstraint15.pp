{ %FAIL }

program tgenconstraint15;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest5TTestRec = TTest5<TTestRec>;

begin

end.
