{ %FAIL }

program tgenconstraint3;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest1ITest1 = TTest1<ITest1>;

begin

end.
